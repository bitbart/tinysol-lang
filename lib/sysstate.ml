open Ast
open Utils

(******************************************************************************)
(*                                 System state                               *)
(******************************************************************************)

(* local environment: 
    maps local variables, plus sender and value
    this is a transient storage, not preserved between calls
*)
type env = ide -> exprval

(* contract state: persistent contract storage, preserved between calls *)
type account_state = {
  balance : int;
  storage : ide -> exprval;
  code : contract option;
}

type frame = {
  callee: addr;            (* called contract *)
  callee_fun: fun_decl;    (* declaration of called function *)
  locals: env list;
}

type sysstate = {
  accounts: addr -> account_state;
  callstack: frame list;
  blocknum: int;
  active: addr list; (* set of all active addresses (for debugging) *)
}

(* execution state of a command *)
type exec_state = 
  | St of sysstate 
  | CmdSt of cmd * sysstate
  | Reverted of string
  | Returned of exprval list



(******************************************************************************)
(*            Functions to access and manipulate system states                *)
(******************************************************************************)

let pop_callstack (st : sysstate) : sysstate = match st.callstack with
    [] -> failwith "empty call stack"
  | _::fl -> { st with callstack = fl } 

let push_callstack (fr : frame) (st : sysstate) : sysstate = 
  { st with callstack = fr :: st.callstack }

let pop_locals (st: sysstate) : sysstate = match st.callstack with
    [] -> failwith "empty call stack"
  | f::fl -> match f.locals with 
    | [] -> failwith "empty locals stack"
    | _::el -> let f' = {f with locals = el } in { st with callstack = f'::fl } 

(* initial (empty) environment *)
let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
    
let bind x v f = fun y -> if y=x then v else f y

(* lookup for variable x in sysstate st *)

let lookup_locals (x : ide) (el : env list) : exprval option =
  List.fold_left
  (fun acc e -> match acc with
    | Some v -> Some v
    | None -> try Some (e x) with _ -> None)
  None
  el

let lookup_var (x : ide) (st : sysstate) : exprval option =
  let fr = List.hd st.callstack in
  (* look up for x in locals stack *)
  match lookup_locals x fr.locals with
  | Some v -> Some v 
  | None -> 
    (* look up for x in storage of callee of top call frame *)
    let cs = st.accounts fr.callee in
    try Some (cs.storage x)
    with _ -> None


let type_of_var x st = match Option.get (lookup_var x st) with
   | Int _  -> IntBT
   | Uint _ -> UintBT
   | Bool _ -> BoolBT
   | Addr _ -> AddrBT false (* check: dynamic typing? *)
   | _ -> failwith "Maps are not handled by this function"
  
let lookup_balance (a : addr) (st : sysstate) : int =
  try (st.accounts a).balance
  with _ -> 0

let lookup_enum_option (st : sysstate) (enum_name : ide) (option_name : ide) : int option = 
  try 
    let a = (List.hd st.callstack).callee in 
    match (st.accounts a).code with
    | Some(Contract(_,edl,_,_)) -> 
      edl
      |> List.filter (fun (Enum(y,_)) -> y=enum_name)
      |> fun edl -> (match edl with [Enum(_,ol)] -> Some ol | _ -> None)  
      |> fun l_opt -> (match l_opt with 
        | None -> None
        | Some ol -> find_index (fun o -> o=option_name) ol)
    | _ -> assert(false) (* should not happen *)
  with _ -> None

let reverse_lookup_enum_option (st : sysstate) (enum_name : ide) (option_index : int) : ide option = 
  try
    let a = (List.hd st.callstack).callee in 
    match (st.accounts a).code with
    | Some(Contract(_,edl,_,_)) -> 
      edl
      |> List.filter (fun (Enum(y,_)) -> y=enum_name)
      |> fun edl -> (match edl with [Enum(_,ol)] -> Some ol | _ -> None)  
      |> fun l_opt -> (match l_opt with 
        | None -> None
        | Some ol -> List.nth_opt ol option_index)
    | _ -> assert(false) (* should not happen *)
  with _ -> None

let exists_account (st : sysstate) (a : addr) : bool =
  try let _ = st.accounts a in true
  with _ -> false

let exists_ide_in_storage (cs : account_state) (x : ide) : bool = 
  try let _ = cs.storage x in true
  with _ -> false

(* 
  Updates the variable x to value x in environment stack el. 
  The variable is searched throughout the environment frames in the stack. 
 *)
let rec update_locals (el : env list) (x : ide) (v : exprval) : env list =
 match el with
  | [] -> failwith (x ^ " not bound in env")
  | e::el' -> 
    try let _ = e x in (* checks if ide x is bound in e *)
      (bind x v e) :: el'
    with _ -> e :: (update_locals el' x v)

(* 
  Updates the variable x to value x in state st. 
  The variable is first searched in the topmost environment, and then in the contract storage. 
 *)
let update_var (st : sysstate) (x : ide) (v : exprval) : sysstate = 
  let fr = List.hd st.callstack in

  (* first tries to update environment if x is bound there *)
   try 
    let fr' = { fr with locals = update_locals fr.locals x v } in
    { st with callstack = fr' :: (List.tl st.callstack)  }
  with _ -> 
    (* if not, tries to update storage of a *)
    let cs = st.accounts fr.callee in
    if exists_ide_in_storage cs x then 
      let cs' = { cs with storage = bind x v cs.storage } in 
      { st with accounts = bind fr.callee cs' st.accounts }
    else failwith (x ^ " not bound in storage of " ^ fr.callee)   


let update_map (st : sysstate) (x:ide) (k:exprval) (v:exprval) : sysstate = 
  let a = (List.hd st.callstack).callee in 
  let cs = st.accounts a in
    if exists_ide_in_storage cs x then 
      match cs.storage x with
      | Map m ->
        let m' = bind k v m in
        let cs' = { cs with storage = bind x (Map m') cs.storage } in 
        { st with accounts = bind a cs' st.accounts }
      | _ -> failwith ("update_map: " ^ x ^ " is not a mapping")
      else failwith ("mapping " ^ x ^ " not bound in storage of " ^ a)   

(******************************************************************************)
(*              Retrieving contracts and functions from state                 *)
(******************************************************************************)

let find_fun_in_contract (Contract(_,_,_,fdl)) (f : ide) : fun_decl option =
  List.fold_left 
  (fun acc fd -> match fd with
    | Constr(_) -> if acc=None && f="constructor" then Some fd else acc  
    | Proc(g,_,_,_,_,_) -> if acc=None && f=g then Some fd else acc
  )
  None
  fdl

let find_var_decl_in_sysstate (st : sysstate) (x : ide) : var_decl option =
    let fr = List.hd st.callstack in
    match (st.accounts fr.callee).code with
    | Some (Contract(_,_,vdl,_)) ->
        let rec find = function
            | [] -> None
            | (vd : var_decl)::tl -> if vd.name = x then Some vd else find tl
        in
        find vdl
    | None -> None

let find_fun_in_sysstate (st : sysstate) (a : addr) (f : ide) = 
  if not (exists_account st a) then
    failwith ("address " ^ a ^ " does not exist")
  else match (st.accounts a).code with
    | None -> None  (* "address " ^ a ^ " is not a contract address" *)
    | Some(c) -> find_fun_in_contract c f 

let in_constructor st =
  match (List.hd st.callstack).callee_fun with
    | Constr _ -> true
    | Proc _ -> false

let get_mutability st = 
  match (List.hd st.callstack).callee_fun with
  | Constr(_,_,m) -> m
  | Proc(_,_,_,_,m,_) -> m

let get_cmd_from_fun = function
  | (Constr(_,c,_)) -> c
  | (Proc(_,_,c,_,_,_)) -> c

let get_var_decls_from_fun = function
  | (Constr(vdl,_,_)) -> vdl
  | (Proc(_,vdl,_,_,_,_)) -> vdl

let bind_fargs_aargs (xl : local_var_decl list) (vl : exprval list) : env =
  if List.length xl <> List.length vl then
    failwith "exec_tx: length mismatch between formal and actual arguments"
  else 
  List.fold_left2 
  (fun acc vd v -> match vd.ty , v with 
    | VarT(IntBT), Int _
    | VarT(UintBT), Uint _
    | VarT(BoolBT), Bool _ 
    | VarT(AddrBT _), Addr _          -> bind vd.name v acc
    | VarT(IntBT), Uint n             -> bind vd.name (Int n) acc
    | VarT(UintBT), Int n when n>=0   -> bind vd.name (Uint n) acc
    | MapT(_),_ -> failwith "Maps cannot be passed as function parameters"
    | _ -> failwith "exec_tx: type mismatch between formal and actual arguments") 
  botenv 
  xl 
  vl 