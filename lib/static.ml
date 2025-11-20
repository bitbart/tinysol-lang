open Ast

type exprtype = 
  | BoolT
  | IntT
  | AddrT

(* TypeError(expression, inferred type, expected type) *)
exception TypeError of expr * exprtype * exprtype;;
exception UndeclaredVar of ide;;

let lookup_type (x : ide) (vdl : var_decl list) : exprtype option =
  if x="msg.sender" then Some AddrT
  else if x="msg.value" then Some IntT else 
  vdl 
  |> List.map (fun vd -> match vd with
    | IntVar x -> IntT,x 
    | UintVar x -> IntT,x
    | BoolVar x-> BoolT,x
    | AddrVar x -> AddrT,x )
  |> List.fold_left
  (fun acc (t,y) -> if acc=None && x=y then Some t else acc)
  None

let merge_var_decls old_vdl new_vdl = new_vdl @ old_vdl  

let rec typecheck_expr (vdl : var_decl list) = function
    True -> BoolT
  | False -> BoolT
  | IntConst _ -> IntT
  | AddrConst _ -> AddrT
  | This -> AddrT (* TODO: make more coherent with Solidity *)
  | Var x -> (match lookup_type x vdl with
    | Some t -> t
    | None -> raise (UndeclaredVar x))
  | BalanceOf(e) -> (match typecheck_expr vdl e with
        AddrT -> IntT
      | _ as t -> raise (TypeError (e,t,AddrT)))
  | Not(e) -> (match typecheck_expr vdl e with
        BoolT -> BoolT
      | _ as t -> raise (TypeError (e,t,BoolT)))
  | And(e1,e2) 
  | Or(e1,e2) ->
    (match (typecheck_expr vdl e1,typecheck_expr vdl e2) with
       (BoolT,BoolT) -> BoolT
     | (t,_) when t<>BoolT -> raise (TypeError (e1,t,BoolT))
     | (_,t) -> raise (TypeError (e2,t,BoolT)))
  | Add(e1,e2) 
  | Sub(e1,e2) 
  | Mul(e1,e2) ->
    (match (typecheck_expr vdl e1,typecheck_expr vdl e2) with
       (IntT,IntT) -> IntT
     | (t,_) when t<>IntT -> raise (TypeError (e1,t,IntT))
     | (_,t) -> raise (TypeError (e2,t,IntT)))
  | Eq(e1,e2)
  | Neq(e1,e2) ->
    (match (typecheck_expr vdl e1,typecheck_expr vdl e2) with
       (t1,t2) when t1=t2-> BoolT
     | (t1,t2) -> raise (TypeError (e2,t2,t1)))
  | Leq(e1,e2)
  | Le(e1,e2)
  | Geq(e1,e2)
  | Ge(e1,e2) ->
    (match (typecheck_expr vdl e1,typecheck_expr vdl e2) with
       (IntT,IntT) -> BoolT
     | (t,IntT) -> raise (TypeError (e1,t,IntT))
     | (_,t) -> raise (TypeError (e2,t,IntT)))
  | IntCast(e) -> (match typecheck_expr vdl e with
        IntT -> IntT
      | _ as t -> raise (TypeError (e,t,IntT)))
  | AddrCast(e) -> (match typecheck_expr vdl e with
      | AddrT -> AddrT
      | IntT -> AddrT
      | _ as t -> raise (TypeError (e,t,IntT))) 
;;

let rec typecheck_cmd (vdl : var_decl list) = function 
    | Skip -> true
    | Assign(x,e) -> 
        typecheck_expr vdl (Var x) =
        typecheck_expr vdl e
      | Seq(c1,c2) -> 
        typecheck_cmd vdl c1 && 
        typecheck_cmd vdl c2
    | If(e,c1,c2) ->
        typecheck_expr vdl e = BoolT &&
        typecheck_cmd vdl c1 && 
        typecheck_cmd vdl c2
    | Send(ercv,eamt) -> 
        typecheck_expr vdl ercv = AddrT &&
        typecheck_expr vdl eamt = IntT
    | Req(e) -> 
        typecheck_expr vdl e = BoolT
    | _ -> failwith "TODO"

  
let typecheck_fun (vdl : var_decl list) = function
  | Constr (al,c,_) -> typecheck_cmd (merge_var_decls vdl al) c
  | Proc (_,al,c,_,__) -> typecheck_cmd (merge_var_decls vdl al) c

let typecheck_contract (Contract(_,vdl,fdl)) =
  List.fold_left 
  (fun acc fd -> acc && typecheck_fun vdl fd)
  true
  fdl  
