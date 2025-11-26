open Ast

(* Types for expressions are a refinement of variable declaration types, 
 * since we want to give more specific types to integer constants in order
 * to have a smoother treatment of int and uint types 
 *)
type exprtype = 
  | BoolET
  | IntET
  | IntConstET
  | UintET
  | UintConstET
  | AddrET of bool
  | MapET of exprtype * exprtype

let rec string_of_exprtype = function
  | BoolET      -> "bool"
  | IntET       -> "int"
  | IntConstET  -> "int const"
  | UintET      -> "uint"
  | UintConstET -> "uint const"
  | AddrET p    -> "address" ^ (if p then " payable" else "")
  | MapET(t1,t2) -> string_of_exprtype t1 ^ " => " ^ string_of_exprtype t2

(* TypeError(expression, inferred type, expected type) *)
exception TypeError of expr * exprtype * exprtype
exception ImmutabilityError of ide
exception UndeclaredVar of ide
exception MultipleDecl of ide

let exprtype_of_decltype = function
  | IntBT  -> IntET
  | UintBT -> UintET
  | BoolBT -> BoolET
  | AddrBT(b) -> AddrET(b)

let lookup_type (x : ide) (vdl : var_decl list) : exprtype option =
  if x="msg.sender" then Some (AddrET false)
  else if x="msg.value" then Some UintET else 
  vdl 
  |> List.map (fun vd -> match vd with
    | VarT(t,_),x  -> (exprtype_of_decltype t),x 
    | MapT(tk,tv),x -> MapET(exprtype_of_decltype tk, exprtype_of_decltype tv),x)
  |> List.fold_left
  (fun acc (t,y) -> if acc=None && x=y then Some t else acc)
  None

let merge_var_decls old_vdl new_vdl = new_vdl @ old_vdl  

let rec dup = function 
  | [] -> None
  | x::l -> if List.mem x l then Some x else dup l

let no_dup_var_decls vdl = 
  vdl 
  |> List.map (fun vd -> match vd with (_,x) -> x) 
  |> dup
  |> fun res -> match res with None -> true | Some x -> raise (MultipleDecl x)  

let no_dup_fun_decls vdl = 
  vdl 
  |> List.map (fun fd -> match fd with 
    | Constr(_) -> "constructor"
    | Proc(f,_,_,_,_) -> f) 
  |> dup
  |> fun res -> match res with None -> true | Some x -> raise (MultipleDecl x)  

let subtype t0 t1 = match t1 with
  | UintConstET -> t0 = t1
  | UintET -> t0 = UintConstET || t0 = t1
  | IntConstET -> t0 = UintConstET || t0 = t1
  | IntET -> t0 = UintConstET || t0 == IntConstET || t0 = t1 (* uint is not convertible to int *)
  | _ -> t0 = t1

let rec typecheck_expr (vdl : var_decl list) = function
    True -> BoolET
  | False -> BoolET
  | IntConst n when n>=0 -> UintConstET
  | IntConst _ -> IntConstET
  | AddrConst _ -> AddrET(false)
  | This -> AddrET(false) (* TODO: make more coherent with Solidity *)
  | BlockNum -> UintConstET
  | Var x -> (match lookup_type x vdl with
    | Some t -> t
    | None -> raise (UndeclaredVar x))
  | MapR(e1,e2) -> (match (typecheck_expr vdl e1, typecheck_expr vdl e2) with
    | MapET(t1k,t1v),t2 when t2 = t1k -> t1v 
    | MapET(t1k,_),t2 -> raise (TypeError (e2,t2,t1k))
    | _ -> failwith "TypeError: map" (* TODO: refine TypeError? *)
    ) 
  | BalanceOf(e) -> (match typecheck_expr vdl e with
        AddrET(_) -> UintET
      | _ as t -> raise (TypeError (e,t,AddrET(false))))
  | Not(e) -> (match typecheck_expr vdl e with
        BoolET -> BoolET
      | _ as t -> raise (TypeError (e,t,BoolET)))
  | And(e1,e2) 
  | Or(e1,e2) ->
    (match (typecheck_expr vdl e1,typecheck_expr vdl e2) with
       (BoolET,BoolET) -> BoolET
     | (t,_) when t<>BoolET -> raise (TypeError (e1,t,BoolET))
     | (_,t) -> raise (TypeError (e2,t,BoolET)))
  | Add(e1,e2)
  | Mul(e1,e2) ->
    (match (typecheck_expr vdl e1,typecheck_expr vdl e2) with
     | (UintConstET,UintConstET) -> UintConstET
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> UintET
     | (t1,t2) when subtype t1 IntConstET && subtype t2 IntConstET -> IntConstET
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> UintET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> IntET
     | (t1,_) when t1<>IntET -> raise (TypeError (e1,t1,IntET))
     | (_,t2) -> raise (TypeError (e2,t2,IntET)))
  | Sub(e1,e2) ->
    (match (typecheck_expr vdl e1,typecheck_expr vdl e2) with
     | (t1,t2) when subtype t1 IntConstET && subtype t2 IntConstET -> IntConstET
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> UintET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> IntET
     | (t1,_) when t1<>IntET -> raise (TypeError (e1,t1,IntET))
     | (_,t2) -> raise (TypeError (e2,t2,IntET)))
  | Eq(e1,e2)
  | Neq(e1,e2) ->
    (match (typecheck_expr vdl e1,typecheck_expr vdl e2) with
       (t1,t2) when t1=t2-> BoolET
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> BoolET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> BoolET
     | (t1,t2) -> raise (TypeError (e2,t2,t1)))
  | Leq(e1,e2)
  | Le(e1,e2)
  | Geq(e1,e2)
  | Ge(e1,e2) ->
    (match (typecheck_expr vdl e1,typecheck_expr vdl e2) with
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> BoolET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> BoolET
     | (t1,IntET) -> raise (TypeError (e1,t1,IntET))
     | (_,t2) -> raise (TypeError (e2,t2,IntET)))
  | IntCast(e) -> (match typecheck_expr vdl e with
      | IntET | UintET -> IntET
      | _ as t -> raise (TypeError (e,t,IntET)))
  | UintCast(e) -> (match typecheck_expr vdl e with
      | IntET | UintET -> UintET
      | _ as t -> raise (TypeError (e,t,IntET)))
  | AddrCast(e) -> (match typecheck_expr vdl e with
      | AddrET(b) -> AddrET(b)
      | IntET -> AddrET(false)
      | _ as t -> raise (TypeError (e,t,IntET))) 
  | PayableCast(e) -> (match typecheck_expr vdl e with
      | AddrET(_) -> AddrET(true)
      | UintConstET -> AddrET(false)
      | _ as t -> raise (TypeError (e,t,IntET))) 

let is_immutable (x : ide) (vdl : var_decls) = 
  List.fold_left (fun acc vd -> match vd with
  | VarT(_,i),y when y=x -> i
  | _ -> acc  
  ) 
  false 
  vdl

let typecheck_local_decls (vdl : var_decls) = 
  List.for_all
  (fun vd -> match vd with 
    | MapT(_),_ -> false
    | _ -> true)
  vdl

let rec typecheck_cmd (is_constr : bool) (vdl : var_decl list) = function 
    | Skip -> true
    | Assign(x,e) -> 
        (* the immutable modifier is not checked for the constructor *)
        if not is_constr && is_immutable x vdl then raise (ImmutabilityError x)
        else
        let te = typecheck_expr vdl e in
        let tx = typecheck_expr vdl (Var x) in
        if subtype te tx then true else raise (TypeError (e,te,tx))
    | MapW(x,ek,ev) ->  
        let tx = typecheck_expr vdl (Var x) in
        let tk = typecheck_expr vdl ek in
        let tv = typecheck_expr vdl ev in
        (match tx with
          | MapET(txk,_) when not (subtype tk txk) -> raise (TypeError (ek,tk,txk)) 
          | MapET(_,txv) when not (subtype tv txv) -> raise (TypeError (ev,tv,txv)) 
          | MapET(_,_) -> true
          | _ -> failwith ("Assignment to non-map variable " ^ x)
        )
    | Seq(c1,c2) -> 
        typecheck_cmd is_constr vdl c1 && 
        typecheck_cmd is_constr vdl c2
    | If(e,c1,c2) ->
        let te = typecheck_expr vdl e in
        if te = BoolET then typecheck_cmd is_constr vdl c1 && typecheck_cmd is_constr vdl c2
        else raise (TypeError (e,te,BoolET))
    | Send(ercv,eamt) -> 
        typecheck_expr vdl ercv = AddrET(true)  (* can only send to payable addresses *) 
        &&
        subtype (typecheck_expr vdl eamt) UintET
    | Req(e) -> 
        let te = typecheck_expr vdl e in
        if te = BoolET then true else raise (TypeError (e,te,BoolET))
    | Block(lvdl,c) -> 
        typecheck_local_decls lvdl &&
        let vdl' = merge_var_decls vdl lvdl in
        typecheck_cmd is_constr vdl' c
    | _ -> failwith "TODO (Call)"

let typecheck_fun (vdl : var_decl list) = function
  | Constr (al,c,_) ->
      no_dup_var_decls al &&
      typecheck_local_decls al && 
      typecheck_cmd true (merge_var_decls vdl al) c
  | Proc (_,al,c,_,__) ->
      no_dup_var_decls al && 
      typecheck_local_decls al &&
      typecheck_cmd false (merge_var_decls vdl al) c

let typecheck_contract (Contract(_,vdl,fdl)) =
  (* no multiply declared variables *)
  no_dup_var_decls vdl
  &&
  (* no multiply declared functions *)
  no_dup_fun_decls fdl
  &&
  List.fold_left 
  (fun acc fd -> acc && typecheck_fun vdl fd)
  true
  fdl  
