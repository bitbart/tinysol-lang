open Ast
open Sysstate

(******************************************************************************)
(*                            Getting set of variables                        *)
(******************************************************************************)

let rec union l1 l2 = match l1 with
    [] -> l2
  | x::l1' -> (if List.mem x l2 then [] else [x]) @ union l1' l2

let rec vars_of_expr = function
  | BoolConst _
  | IntConst _
  | IntVal _
  | UintVal _ 
  | AddrConst _
  | This 
  | BlockNum 
  | EnumOpt _ -> []               
  | Var x -> [x]
  | BalanceOf e
  | Not e
  | IntCast e
  | UintCast e
  | AddrCast e
  | PayableCast e ->  vars_of_expr e
  | MapR(e1,e2) 
  | And(e1,e2) 
  | Or(e1,e2) 
  | Add(e1,e2)
  | Sub(e1,e2)
  | Mul(e1,e2) 
  | Eq(e1,e2) 
  | Neq(e1,e2) 
  | Leq(e1,e2) 
  | Lt(e1,e2)
  | Geq(e1,e2) 
  | Gt(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)                    
  | IfE(e1,e2,e3) -> union (vars_of_expr e1) (union (vars_of_expr e2) (vars_of_expr e3))
  | EnumCast(x,e) ->  union [x] (vars_of_expr e)
  | FunCall(e_to,_,e_value,e_args) -> union (vars_of_expr e_to) (union (vars_of_expr e_value) 
    (List.fold_left (fun acc ea -> union acc (vars_of_expr ea)) [] e_args))
  | ExecFunCall(c) ->  vars_of_cmd c

and vars_of_cmd = function
  | Skip -> []
  | Decl(vd) -> [vd.name]
  | Assign(x,e) -> union [x] (vars_of_expr e)
  | MapW(x,ek,ev) -> union [x] (union (vars_of_expr ek) (vars_of_expr ev))
  | Seq(c1,c2) -> union (vars_of_cmd c1) (vars_of_cmd c2)
  | If(e,c1,c2) -> union (vars_of_expr e) (union (vars_of_cmd c1) (vars_of_cmd c2))
  | Send(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)
  | Req(e) -> vars_of_expr e    
  | Return(e) -> vars_of_expr e               
  | Block(_,c) -> vars_of_cmd c
  | ExecBlock(c) 
  | ExecProcCall(c) -> vars_of_cmd c
  | ProcCall(e_to,_,e_value,e_args) -> union (vars_of_expr e_to) (union (vars_of_expr e_value) 
    (List.fold_left (fun acc ea -> union acc (vars_of_expr ea)) [] e_args))
  
let vars_of_contract (Contract(_,_,vdl,_)) : ide list = 
  List.fold_left (fun acc (vd : var_decl) -> vd.name::acc ) [] vdl 


(******************************************************************************)
(*                         Converting syntax to strings                       *)
(******************************************************************************)

let string_of_exprval = function
    Bool b -> string_of_bool b
  | Int n
  | Uint n -> string_of_int n
  | Addr s -> s
  | Map _  -> "<map>" (* do not expand map *) 

let string_of_visibility = function
  | Public    -> "public"
  | Private   -> "private"
  | Internal  -> "internal"
  | External  -> "external"

let string_of_mutability = function
  | Pure    -> "pure"
  | View    -> "view"
  | NonPayable -> ""
  | Payable -> "payable"

let string_of_args = List.fold_left (fun s a -> s ^ (if s<>"" then "," else "") ^ (string_of_exprval a)) ""

let rec string_of_expr = function
  | BoolConst b -> if b then "true" else "false"
  | IntConst n 
  | IntVal n
  | UintVal n -> string_of_int n
  | AddrConst a -> "\"" ^ a ^ "\""
  | This -> "this"
  | BlockNum -> "block.num"
  | Var x -> x
  | MapR(e1,e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | BalanceOf e -> string_of_expr e ^ ".balance"
  | Not e -> "!" ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " && " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " || " ^ string_of_expr e2
  | Add(e1,e2) -> string_of_expr e1 ^ "+" ^ string_of_expr e2
  | Sub(e1,e2) -> string_of_expr e1 ^ "-" ^ string_of_expr e2
  | Mul(e1,e2) -> string_of_expr e1 ^ "*" ^ string_of_expr e2
  | Eq(e1,e2) -> string_of_expr e1 ^ "==" ^ string_of_expr e2
  | Neq(e1,e2) -> string_of_expr e1 ^ "!=" ^ string_of_expr e2
  | Leq(e1,e2) -> string_of_expr e1 ^ "<=" ^ string_of_expr e2
  | Lt(e1,e2) -> string_of_expr e1 ^ "<" ^ string_of_expr e2                    
  | Geq(e1,e2) -> string_of_expr e1 ^ ">=" ^ string_of_expr e2
  | Gt(e1,e2) -> string_of_expr e1 ^ ">" ^ string_of_expr e2
  | IfE(e1,e2,e3) -> "(" ^ string_of_expr e1 ^ ")?" ^ string_of_expr e2 ^ ":" ^ string_of_expr e3
  | IntCast(e) -> "int(" ^ string_of_expr e ^ ")"
  | UintCast(e) -> "int(" ^ string_of_expr e ^ ")"
  | AddrCast(e) -> "address(" ^ string_of_expr e ^ ")"
  | PayableCast(e) -> "payable(" ^ string_of_expr e ^ ")"
  | EnumOpt(x,o) -> x ^ "." ^ o
  | EnumCast(x,e) -> x ^ "(" ^ string_of_expr e ^ ")"
  | FunCall(e_to,f,e_value,e_args) -> string_of_expr e_to ^ "." ^ f ^ 
    "{value:" ^ string_of_expr e_value ^ "}" ^
    "(" ^ (List.fold_left (fun acc ea -> acc ^ (if acc="" then "" else ",") ^ string_of_expr ea) "" e_args) ^ ")"
  | ExecFunCall(c) -> "<" 
    ^ string_of_cmd c 
    ^ ">"

and string_of_cmd = function
  | Skip -> "skip;"
  | Decl d -> string_of_local_var_decl d ^ " ;"
  | Assign(x,e) -> x ^ " = " ^ string_of_expr e ^ ";"
  | MapW(x,ek,ev) -> x ^ "[" ^ string_of_expr ek ^ "] = " ^ string_of_expr ev ^ ";"
  | Seq(c1,c2) -> string_of_cmd c1 ^ " " ^ string_of_cmd c2
  | If(e,c1,c2) -> "if (" ^ string_of_expr e ^ ") " ^ string_of_cmd c1 ^ " else " ^ string_of_cmd c2 ^ ""
  | Send(e1,e2) -> string_of_expr e1 ^ ".transfer(" ^ (string_of_expr e2) ^ ");"
  | Req(e) -> "require " ^ string_of_expr e ^ ";"
  | Return e -> "return " ^ string_of_expr e ^ ";"
  | Block(vdl,c) -> "{" 
    ^ List.fold_left (fun s d -> s ^ string_of_local_var_decl d ^ "; ") "" vdl 
    ^ string_of_cmd c 
    ^ "}"
  | ExecBlock(c) -> "{" 
    ^ string_of_cmd c 
    ^ "}"
  | ProcCall(e_to,f,e_value,e_args) -> string_of_expr e_to ^ "." ^ f ^ 
    "{value:" ^ string_of_expr e_value ^ "}" ^
    "(" ^ (List.fold_left (fun acc ea -> acc ^ (if acc="" then "" else ",") ^ string_of_expr ea) "" e_args) ^ ")"
  | ExecProcCall(c) -> "{" 
    ^ string_of_cmd c 
    ^ "}"

and string_of_base_type = function
| IntBT  -> "int"
| UintBT -> "uint"
| BoolBT -> "bool"
| AddrBT p -> "address" ^ (if p then " payable" else "")
| CustomBT x -> x
| EnumBT x -> x

and string_of_var_type = function
| VarT(t) -> string_of_base_type t
| MapT(tk,tv) -> "mapping (" ^ string_of_base_type tk ^ " => " ^ string_of_base_type tv ^ ")"

and string_of_var_decl (vd : var_decl) : string = 
  string_of_var_type vd.ty ^ " " ^ 
  string_of_visibility vd.visibility ^ " " ^ 
  (if vd.immutable then "immutable " else "") ^
  vd.name

and string_of_local_var_decl (vd : local_var_decl) : string = 
  string_of_var_type vd.ty ^ " " ^ 
  vd.name

let string_of_var_decls = List.fold_left (fun s d -> s ^ (if s<>"" then ";\n  " else "  ") ^ string_of_var_decl d) ""

let string_of_local_var_decls = List.fold_left (fun s d -> s ^ (if s<>"" then ";\n  " else "  ") ^ string_of_local_var_decl d) ""

let string_of_fun_args = List.fold_left (fun s d -> s ^ (if s<>"" then ", " else "") ^ string_of_local_var_decl d) ""

let add_space w = if w="" then "" else w ^ " "

let string_of_fun_decl = function 
  | Proc(f,al,c,v,m,ret) -> 
    "function " ^ f ^ "(" ^ (string_of_fun_args al) ^ ") " ^
    add_space (string_of_visibility v) ^
    add_space (string_of_mutability m) ^ 
    (match ret with None -> "" | Some t -> "returns(" ^ string_of_base_type t ^ ") ") ^ 
    "{" ^ string_of_cmd c ^ "}\n"
  | Constr(al,c,m) ->       
    "constructor " ^ "(" ^ (string_of_fun_args al) ^ ") " ^
    add_space (string_of_mutability m) ^ 
    "{" ^ string_of_cmd c ^ "}\n"

let string_of_fun_decls = List.fold_left (fun s d -> s ^ (if s<>"" then "  " else " ") ^ string_of_fun_decl d) ""

let string_of_enum_decl (Enum(x,ol)) = 
  "enum " ^ x ^ "{" ^
  List.fold_left(fun acc o -> if acc="" then o else acc ^ ", " ^ o) "" ol ^
  "}"

let string_of_enum_decls = List.fold_left (fun s d -> s ^ (if s<>"" then "  " else " ") ^ string_of_enum_decl d) ""


let string_of_contract (Contract(c,edl,vdl,fdl)) = 
  "contract " ^ c ^ 
  " {\n" ^ 
  (let s = string_of_enum_decls edl in if s="" then "" else s ^ ";\n ") ^ 
  (let s = string_of_var_decls vdl in if s="" then "" else s ^ ";\n ") ^ 
  (string_of_fun_decls fdl) ^ 
  " }"

let string_of_env e vl = 
  let rec helper e vl = match vl with 
  | [] -> ""
  | x::vl' -> try (x ^ "->" ^ string_of_exprval (e x))
    with _ -> helper e vl' 
  in "{" ^ helper e vl ^ "}"

let string_of_locals el vl = 
  let rec helper el vl = match el with
    [] -> ""
  | [e] -> (string_of_env e vl)
  | e::el' -> (string_of_env e vl) ^ ";" ^ (helper el' vl)
in "[" ^ (helper el vl) ^ "]" 

let rec range a b = if b<a then [] else a::(range (a+1) b);;

let string_of_storage (stg : ide -> exprval) (xl : ide list) =
  List.fold_left (fun acc x -> acc ^ x ^ "=" ^ (string_of_exprval (stg x)) ^ "; ") "" xl

let string_of_account_state accst =
  "{ " ^
  "balance=" ^ string_of_int accst.balance ^ "; " ^
  (match accst.code with 
  | None -> ""
  | Some src -> string_of_storage accst.storage (vars_of_contract src)) ^
  "}"

let string_of_accounts (st : sysstate) =
  "[" ^ 
  (List.fold_left (fun acc a -> acc ^ a ^ " -> " ^ (string_of_account_state (st.accounts a)) ^ " ") "" st.active) ^ 
  "]"

let string_of_frame (f : frame) (vl : ide list)=
  f.callee ^ " " ^ (string_of_locals f.locals vl)

let string_of_callstack (fl : frame list) (vl : ide list) = 
    let rec helper fl vl = match fl with
    [] -> ""
  | [f] -> (string_of_frame f vl)
  | f::fl' -> (string_of_frame f vl) ^ ";" ^ (helper fl' vl)
in "[" ^ (helper fl vl) ^ "]" 

let string_of_sysstate (evl : ide list) (st : sysstate) =
  "accounts: " ^ 
  string_of_accounts st ^
  if evl=[] then "" else 
  ("\n" ^
  "callstack: " ^
  string_of_callstack st.callstack evl)

let string_of_execstate evl = function
  | St st -> string_of_sysstate evl st
  | Reverted msg -> "revert: " ^ msg
  | Returned v -> "returned " ^ string_of_exprval v
  | CmdSt (c,st) -> 
      "cmd: " ^ (string_of_cmd c) ^ "\n" ^ 
      (string_of_sysstate evl st) 

let string_of_trace stl = match stl with
  [] -> ""
| St _::_ -> ""
| Reverted msg :: _ -> "revert " ^ msg
| Returned v :: _ -> "returned " ^ string_of_exprval v 
| CmdSt (c,_)::_ -> let evl = vars_of_cmd c in  
  let rec helper stl = (match stl with
    [] -> ""
  | [st] -> (string_of_execstate evl st)
  | st::l -> (string_of_execstate evl st) ^ "\n--->\n" ^ helper l)
in helper stl

let string_of_transaction tx =
  tx.txsender ^ ":" ^ tx.txto ^ "." ^ tx.txfun ^
  "{value: " ^ (string_of_int tx.txvalue) ^ "}" ^ 
  "(" ^ string_of_args tx.txargs ^ ")" 
