open Ast
open Sysstate
open Utils


(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

exception TypeError of string
exception NoRuleApplies

let rec step_expr (e,st) = match e with
  | e when is_val e -> raise NoRuleApplies

  | This -> ((AddrConst (List.hd(st.callstack)).callee), st)

  | BlockNum -> (IntConst st.blocknum, st)

  | Var x -> (expr_of_exprval (Option.get (lookup_var x st)), st)  

  | MapR(Var x,e2) when is_val e2 -> (match Option.get (lookup_var x st) with
    | Map m -> (expr_of_exprval (m (exprval_of_expr e2)), st)
    | _ -> failwith "step_expr: wrong type checking of map?")
  | MapR(Var x,e2) ->
    let (e2', st') = step_expr (e2, st) in (MapR(Var x,e2'), st')
  | MapR(e1,e2) ->
    let (e1', st') = step_expr (e1, st) in (MapR(e1',e2), st')

  | BalanceOf e when is_val e -> 
    let b = addr_of_expr e in (UintVal (lookup_balance b st), st)
  | BalanceOf e -> 
    let (e', st') = step_expr (e, st) in (BalanceOf e', st')

  | Not(e) when is_val e -> 
    let b = bool_of_expr e in (BoolConst (not b), st)
  | Not(e) -> 
    let (e', st') = step_expr (e, st) in (Not e', st')

  | And(e1,e2) when is_val e1 && is_val e2 ->
    let (b1,b2) = bool_of_expr e1,bool_of_expr e2 in 
    (BoolConst (b1 && b2), st)         
  | And(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (And(e1,e2'), st')
  | And(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (And(e1',e2), st')

  | Or(e1,e2) when is_val e1 && is_val e2 ->
    let (b1,b2) = bool_of_expr e1,bool_of_expr e2 in 
    (BoolConst(b1 || b2), st)
  | Or(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (Or(e1,e2'), st')
  | Or(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (Or(e1',e2), st')

  | Add(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
    | (IntConst n1, IntConst n2) -> (IntConst (n1+n2), st)
    | (IntConst n1, UintVal n2) when n1>=0 -> (UintVal (n1+n2), st)
    | (UintVal n1, IntConst n2) when n2>=0 -> (UintVal (n1+n2), st)
    | (IntConst n1, IntVal n2) -> (IntVal (n1+n2), st)
    | (IntVal n1, IntConst n2) -> (IntVal (n1+n2), st)
    | (UintVal n1, UintVal n2) -> (UintVal (n1+n2), st)
    | (IntVal n1, IntVal n2)   -> (IntVal (n1+n2), st)
    | _ -> raise (TypeError "Add: type mismatch between the operands"))
  | Add(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (Add(e1,e2'), st')
  | Add(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (Add(e1',e2), st')

  | Sub(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
    | (IntConst n1, IntConst n2) -> (IntConst (n1-n2), st)
    | (IntConst n1, UintVal n2) when n1-n2>=0 -> (UintVal (n1-n2), st)
    | (UintVal n1, IntConst n2) when n1-n2>=0 -> (UintVal (n1-n2), st)
    | (IntConst n1, IntVal n2) -> (IntVal (n1-n2), st)
    | (IntVal n1, IntConst n2) -> (IntVal (n1-n2), st)
    | (UintVal n1, UintVal n2) when n1-n2>=0 -> (UintVal (n1-n2), st)
    | (IntVal n1, IntVal n2) -> (IntVal (n1-n2), st)
    | _ -> raise (TypeError "Sub: type mismatch between the operands"))
  | Sub(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (Sub(e1,e2'), st')
  | Sub(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (Sub(e1',e2), st')

  | Mul(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
    | (IntConst n1, IntConst n2) -> (IntConst (n1*n2), st)
    | (IntConst n1, UintVal n2) when n1>=0 -> (UintVal (n1*n2), st)
    | (UintVal n1, IntConst n2) when n2>=0 -> (UintVal (n1*n2), st)
    | (IntConst n1, IntVal n2) -> (IntVal (n1*n2), st)
    | (IntVal n1, IntConst n2) -> (IntVal (n1*n2), st)
    | (UintVal n1, UintVal n2) -> (UintVal (n1*n2), st)
    | (IntVal n1, IntVal n2) -> (IntVal (n1*n2), st)
    | _ -> raise (TypeError "Mul: type mismatch between the operands"))
  | Mul(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (Mul(e1,e2'), st')
  | Mul(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (Mul(e1',e2), st')
  
    
  | Div(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
    | (IntConst n1, IntConst n2) -> 
        if n2 = 0 then failwith "Runtime Error: Division by zero" 
        else (IntConst (n1/n2), st)
    | (IntConst n1, UintVal n2) when n1>=0 -> 
        if n2 = 0 then failwith "Runtime Error: Division by zero" 
        else (UintVal (n1/n2), st)
    | (UintVal n1, IntConst n2) when n2>=0 -> 
        if n2 = 0 then failwith "Runtime Error: Division by zero" 
        else (UintVal (n1/n2), st)
    | (IntConst n1, IntVal n2) -> 
        if n2 = 0 then failwith "Runtime Error: Division by zero" 
        else (IntVal (n1/n2), st)
    | (IntVal n1, IntConst n2) -> 
        if n2 = 0 then failwith "Runtime Error: Division by zero" 
        else (IntVal (n1/n2), st)
    | (UintVal n1, UintVal n2) -> 
        if n2 = 0 then failwith "Runtime Error: Division by zero" 
        else (UintVal (n1/n2), st)
    | (IntVal n1, IntVal n2) -> 
        if n2 = 0 then failwith "Runtime Error: Division by zero" 
        else (IntVal (n1/n2), st)
    | _ -> raise (TypeError "Div: type mismatch between the operands"))
  | Div(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (Div(e1,e2'), st')
  | Div(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (Div(e1',e2), st')

  | Eq(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2)
      | (IntConst n1,IntVal n2)
      | (IntVal n1,IntConst n2)
      | (UintVal n1,UintVal n2)
      | (IntVal n1,IntVal n2)               -> (BoolConst(n1=n2), st)
      | (UintVal n1,IntConst n2) when n2>=0 -> (BoolConst(n1=n2), st)
      | (IntConst n1,UintVal n2) when n1>=0 -> (BoolConst(n1=n2), st)
      | (AddrConst a1,AddrConst a2)         -> (BoolConst(a1=a2), st)
      | (BoolConst b1,BoolConst b2)         -> (BoolConst(b1=b2), st)
      | _ -> raise (TypeError "Eq"))
  | Eq(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (Eq(e1,e2'), st')
  | Eq(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (Eq(e1',e2), st')

  | Neq(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2)
      | (IntConst n1,IntVal n2)
      | (IntVal n1,IntConst n2)
      | (UintVal n1,UintVal n2)
      | (IntVal n1,IntVal n2)               -> (BoolConst(n1<>n2), st)
      | (UintVal n1,IntConst n2) when n2>=0 -> (BoolConst(n1<>n2), st)
      | (IntConst n1,UintVal n2) when n1>=0 -> (BoolConst(n1<>n2), st)
      | (AddrConst a1,AddrConst a2) -> (BoolConst(a1<>a2), st)
      | (BoolConst b1,BoolConst b2) -> (BoolConst(b1<>b2), st)
      | _ -> raise (TypeError "Neq"))
  | Neq(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (Neq(e1,e2'), st')
  | Neq(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (Neq(e1',e2), st')

  | Leq(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2)
      | (IntConst n1,IntVal n2)
      | (IntVal n1,IntConst n2)
      | (UintVal n1,UintVal n2)
      | (IntVal n1,IntVal n2)               -> (BoolConst(n1<=n2), st)
      | (UintVal n1,IntConst n2) when n2>=0 -> (BoolConst(n1<=n2), st)
      | (IntConst n1,UintVal n2) when n1>=0 -> (BoolConst(n1<=n2), st)
      | _ -> raise (TypeError "Leq"))
  | Leq(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (Leq(e1,e2'), st')
  | Leq(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (Leq(e1',e2), st')

  | Lt(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2)
      | (IntConst n1,IntVal n2)
      | (IntVal n1,IntConst n2)
      | (UintVal n1,UintVal n2)
      | (IntVal n1,IntVal n2)               -> (BoolConst(n1<n2), st)
      | (UintVal n1,IntConst n2) when n2>=0 -> (BoolConst(n1<n2), st)
      | (IntConst n1,UintVal n2) when n1>=0 -> (BoolConst(n1<n2), st)
      | _ -> raise (TypeError "Lt"))
  | Lt(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (Lt(e1,e2'), st')
  | Lt(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (Lt(e1',e2), st')

  | Geq(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2)
      | (IntConst n1,IntVal n2)
      | (IntVal n1,IntConst n2)
      | (UintVal n1,UintVal n2)
      | (IntVal n1,IntVal n2)               -> (BoolConst(n1>=n2), st)
      | (UintVal n1,IntConst n2) when n2>=0 -> (BoolConst(n1>=n2), st)
      | (IntConst n1,UintVal n2) when n1>=0 -> (BoolConst(n1>=n2), st)
      | _ -> raise (TypeError "Geq"))
  | Geq(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (Geq(e1,e2'), st')
  | Geq(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (Geq(e1',e2), st')

  | Gt(e1,e2) when is_val e1 && is_val e2 -> (match e1,e2 with
      | (IntConst n1,IntConst n2)
      | (IntConst n1,IntVal n2)
      | (IntVal n1,IntConst n2)
      | (UintVal n1,UintVal n2)
      | (IntVal n1,IntVal n2)               -> (BoolConst(n1>n2), st)
      | (UintVal n1,IntConst n2) when n2>=0 -> (BoolConst(n1>n2), st)
      | (IntConst n1,UintVal n2) when n1>=0 -> (BoolConst(n1>n2), st)
      | _ -> raise (TypeError "Gt"))
  | Gt(e1,e2) when is_val e1 ->
    let (e2', st') = step_expr (e2, st) in (Gt(e1,e2'), st')
  | Gt(e1,e2) -> 
    let (e1', st') = step_expr (e1, st) in (Gt(e1',e2), st')

  | IfE(e1,e2,e3) when is_val e1 -> 
    let b1 = bool_of_expr e1 in ((if b1 then e2 else e3), st)
  | IfE(e1,e2,e3) -> 
    let (e1', st') = step_expr (e1, st) in (IfE(e1',e2,e3), st')    

  | IntCast(e) when is_val e -> (match e with
    | IntConst n -> (IntConst n, st)
    | IntVal n
    | UintVal n  -> (IntVal n, st)
    | _ -> raise (TypeError "Cast of a non-integer expression"))
  | IntCast(e) -> 
    let (e', st') = step_expr (e, st) in (IntCast(e'), st')    

  | UintCast(e) when is_val e -> (match e with
    | IntConst n when n>=0 -> (IntConst n, st)
    | IntVal n when n>=0   -> (UintVal n, st)
    | UintVal n            -> (UintVal n, st)
    | _ -> raise (TypeError "Cast of a non-uint expression"))
  | UintCast(e) -> 
    let (e', st') = step_expr (e, st) in (UintCast(e'), st')    

  | AddrCast(e) when is_val e -> 
      let b = addr_of_expr e in (AddrConst b, st)
  | AddrCast(e) -> 
    let (e', st') = step_expr (e, st) in (AddrCast(e'), st')    

  | PayableCast(e) when is_val e -> 
      let b = addr_of_expr e in (AddrConst b, st) (* payable cast is only implemented by the type checker *)
  | PayableCast(e) -> 
    let (e', st') = step_expr (e, st) in (PayableCast(e'), st')    

  | EnumOpt(x,o) -> (match lookup_enum_option st x o with
    | Some n -> (IntConst n, st)
    | None -> failwith "Enum lookup failed (bug in typechecking?)")

  | EnumCast(x,e) when is_val e -> (match exprval_of_expr e with
      | Int n 
      | Uint n -> (match reverse_lookup_enum_option st x n with
        | Some _ -> (IntConst n, st)
        | None -> raise (TypeError "EnumCast"))
      | _ -> raise (TypeError "EnumCast: expression is not an Int")
      )
  | EnumCast(x,e) -> 
    let (e', st') = step_expr (e, st) in (EnumCast(x,e'), st')    

  | ContractCast(_,e) when is_val e -> (match exprval_of_expr e with
      | Addr a -> (AddrConst a, st)   
      | _ -> raise (TypeError "ContractCast: expression is not an Addr")
      )
  | ContractCast(x,e) -> 
    let (e', st') = step_expr (e, st) in (ContractCast(x,e'), st')    

  | FunCall(e_to,f,e_value,e_args) when is_val e_to && is_val e_value && List.for_all is_val e_args ->  
    (* retrieve function declaration *)
    let txfrom = addr_of_exprval (Addr (List.hd(st.callstack)).callee) in 
    let txto = addr_of_expr e_to in
    let txvalue  = int_of_expr e_value in
    let txargs = List.map (fun arg -> exprval_of_expr arg) e_args in
    if lookup_balance txfrom st < txvalue then 
      failwith ("sender has not sufficient wei balance")
    else
    let from_state = 
      { (st.accounts txfrom) with balance = (st.accounts txfrom).balance - txvalue } in
    let to_state  = 
      { (st.accounts txto) with balance = (st.accounts txto).balance + txvalue } in 
    let fdecl = Option.get (find_fun_in_sysstate st txto f) in  
    (* setup new callstack frame *)
    let xl = get_var_decls_from_fun fdecl in
    let xl',vl' =
      { ty=VarT(AddrBT false); name="msg.sender"; } :: 
      { ty=VarT(UintBT); name="msg.value"; } :: xl,
      Addr txfrom :: 
      Uint txvalue :: txargs
    in
    let fr' = { callee = txto; locals = [bind_fargs_aargs xl' vl'] } in 
    let st' = { accounts = st.accounts 
                  |> bind txfrom from_state
                  |> bind txto to_state; 
                callstack = fr' :: st.callstack;
                blocknum = st.blocknum;
                active = st.active } in
    let c = get_cmd_from_fun fdecl in
    (ExecFunCall(c), st')

  | FunCall(e_to,f,e_value,e_args) when is_val e_to && is_val e_value -> 
    let (e_args', st') = step_expr_list (e_args, st) in 
    (FunCall(e_to,f,e_value,e_args'), st')

  | FunCall(e_to,f,e_value,e_args) when is_val e_to -> 
    let (e_value', st') = step_expr (e_value, st) in 
    (FunCall(e_to,f,e_value',e_args), st')
  
  | FunCall(e_to,f,e_value,e_args) -> 
    let (e_to', st') = step_expr (e_to, st) in
    (FunCall(e_to',f,e_value,e_args), st')

  | ExecFunCall(c) -> (match step_cmd (CmdSt(c,st)) with
    | St _ -> failwith "function terminated without return"
    | Reverted s -> failwith s
    | Returned vl -> (match vl with
      | [v] -> (expr_of_exprval v, pop_callstack st)
      | _ -> failwith "multiple return values not supported"
    )
    | CmdSt(c',st') -> (ExecFunCall(c'),st')
    )

  | _ -> assert(false)

(* Perform a single evaluation step on a list of actual arguments *)
and step_expr_list (el, st) = match el with
  | [] -> (el, st)
  | e::tl when is_val e -> 
    let (el',st') = step_expr_list (tl, st) in (e::el', st')
  | e::tl -> 
    let (e',st') = step_expr (e, st) in (e'::tl, st')


(******************************************************************************)
(*                       Small-step semantics of commands                     *)
(******************************************************************************)

and step_cmd = function
    St _ -> raise NoRuleApplies

  | Reverted s -> Reverted s

  | Returned v -> Returned v

  | CmdSt(c,st) -> (match c with

    | Skip -> St st

    | Assign(x,e) when is_val e -> 
        St (update_var st x (exprval_of_expr_typechecked e (type_of_var x st)))
        
    | Assign(x,e) -> 
      let (e', st') = step_expr (e, st) in CmdSt(Assign(x,e'), st')

    | Decons(_) -> failwith "TODO: multiple return values"

    | MapW(x,ek,ev) when is_val ek && is_val ev ->
        St (update_map st x (exprval_of_expr ek) (exprval_of_expr ev))
    | MapW(x,ek,ev) when is_val ek -> 
      let (ev', st') = step_expr (ev, st) in 
      CmdSt(MapW(x,ek,ev'), st')
    | MapW(x,ek,ev) -> 
      let (ek', st') = step_expr (ek, st) in 
      CmdSt(MapW(x,ek',ev), st')
    
    | Seq(c1,c2) -> (match step_cmd (CmdSt(c1,st)) with
        | St st1 -> CmdSt(c2,st1)
        | Reverted s -> Reverted s
        | Returned v -> Returned v
        | CmdSt(c1',st1) -> CmdSt(Seq(c1',c2),st1))

    | If(e,c1,c2) when is_val e -> (match exprval_of_expr e with
        | Bool true  -> CmdSt(c1,st)
        | Bool false -> CmdSt(c2,st)
        | _ -> Reverted "if: type error")
    | If(e,c1,c2) -> 
        let (e', st') = step_expr (e, st) in
        CmdSt(If(e',c1,c2), st')

    | Send(ercv,eamt) when is_val ercv && is_val eamt -> 
        let rcv = addr_of_expr ercv in 
        let amt = int_of_expr eamt in
        let from = (List.hd st.callstack).callee in 
        let from_bal = (st.accounts from).balance in
        if from_bal<amt then Reverted "insufficient balance" else
        let from_state =  { (st.accounts from) with balance = from_bal - amt } in
        if exists_account st rcv then
          let rcv_state = { (st.accounts rcv) with balance = (st.accounts rcv).balance + amt } in
           St { st with accounts = st.accounts |> bind rcv rcv_state |> bind from from_state}
        else
          let rcv_state = { balance = amt; storage = botenv; code = None; } in
          St { st with accounts = st.accounts |> bind rcv rcv_state |> bind from from_state; active = rcv::st.active }

    | Send(ercv,eamt) when is_val ercv -> 
        let (eamt', st') = step_expr (eamt, st) in
        CmdSt(Send(ercv,eamt'), st')

    | Send(ercv,eamt) -> 
        let (ercv', st') = step_expr (ercv, st) in
        CmdSt(Send(ercv',eamt), st')

    | Req(e) when is_val e -> 
        let b = bool_of_expr e in 
        if b then St st else Reverted "require condition is false"
    | Req(e) -> 
      let (e', st') = step_expr (e, st) in CmdSt(Req(e'), st')

    | Return(el) when List.for_all is_val el -> Returned (List.map exprval_of_expr el) 
    | Return(el) -> (match el with
      | [e] -> let (e', st') = step_expr (e, st) in CmdSt(Return([e']), st')
      | _ -> failwith "TODO: multiple return values not supported")
    
    | Block(vdl,c) ->
        let r' = List.fold_left (fun acc vd ->
          match vd.ty with
            | VarT(IntBT)        -> acc |> bind vd.name (Int 0) 
            | VarT(UintBT)       -> acc |> bind vd.name (Uint 0)
            | VarT(BoolBT)       -> acc |> bind vd.name (Bool false)
            | VarT(AddrBT _)     -> acc |> bind vd.name (Addr "0")
            | VarT(EnumBT _)     -> acc |> bind vd.name (Uint 0)
            | VarT(ContractBT _) -> acc |> bind vd.name (Addr "0")
            | VarT(UnknownBT _)  -> assert(false) (* should not happen after preprocessing *)
            | MapT(_) -> failwith "mappings cannot be used in local declarations" 
        ) botenv vdl in
        let fr,frl = (List.hd st.callstack),(List.tl st.callstack) in
        let fr' = { fr with locals = r'::fr.locals } in
        CmdSt(ExecBlock c, { st with callstack = fr'::frl })

    | ExecBlock(c) -> (match step_cmd (CmdSt(c,st)) with
        | St st -> St (pop_locals st)
        | Reverted s -> Reverted s
        | Returned v -> Returned v
        | CmdSt(c1',st1) -> CmdSt(ExecBlock(c1'),st1))

    | Decl _ -> assert(false) (* should not happen after blockify *)

    | ProcCall(e_to,f,e_value,e_args) when is_val e_to && is_val e_value && List.for_all is_val e_args ->
        (* retrieve function declaration *)
        let txfrom = (List.hd (st.callstack)).callee in 
        let txto   = addr_of_expr e_to in
        let txvalue  = int_of_expr e_value in
        let txargs = List.map (fun arg -> exprval_of_expr arg) e_args in
        if lookup_balance txfrom st < txvalue then 
          Reverted ("sender " ^ txfrom ^ " has not sufficient wei balance")
        else
        let from_state = 
          { (st.accounts txfrom) with balance = (st.accounts txfrom).balance - txvalue } in
        let to_state  = 
          { (st.accounts txto) with balance = (st.accounts txto).balance + txvalue } in 
        let fdecl = Option.get (find_fun_in_sysstate st txto f) in  
        (* setup new stack frame TODO *)
        let xl = get_var_decls_from_fun fdecl in
        let xl',vl' =
          { ty=VarT(AddrBT false); name="msg.sender"; } :: 
          { ty=VarT(UintBT); name="msg.value"; } :: xl,
          Addr txfrom :: 
          Uint txvalue :: txargs
        in
        let fr' = { callee = txto; locals = [bind_fargs_aargs xl' vl'] } in
        let st' = { accounts = st.accounts 
                      |> bind txfrom from_state
                      |> bind txto to_state; 
                    callstack = fr' :: st.callstack;
                    blocknum = st.blocknum;
                    active = st.active } in
        let c = get_cmd_from_fun fdecl in
        CmdSt(ExecProcCall(c), st')

    | ProcCall(e_to,f,e_value,e_args) when is_val e_to && is_val e_value -> 
      let (e_args', st') = step_expr_list (e_args, st) in 
      CmdSt(ProcCall(e_to,f,e_value,e_args'), st')

    | ProcCall(e_to,f,e_value,e_args) when is_val e_to -> 
      let (e_value', st') = step_expr (e_value, st) in 
      CmdSt(ProcCall(e_to,f,e_value',e_args), st')

    | ProcCall(e_to,f,e_value,e_args) -> 
      let (e_to', st') = step_expr (e_to, st) in 
      CmdSt(ProcCall(e_to',f,e_value,e_args), st')

    | ExecProcCall(c) -> (match step_cmd (CmdSt(c,st)) with
      | St st -> St (pop_callstack st)
      | Reverted s -> Reverted s
      | Returned _ -> St (pop_callstack st)
      | CmdSt(c1',st1) -> CmdSt(ExecProcCall(c1'),st1))
  )

(* Recursively evaluate expression until it reaches a value (might not terminate) *)

let rec eval_expr (st : sysstate) (e : expr) : exprval = 
  if is_val e then exprval_of_expr e
  else let (e', st') = step_expr (e, st) in eval_expr st' e'  

let default_var_value = function 
| IntBT       -> Int 0
| UintBT      -> Uint 0
| BoolBT      -> Bool false
| AddrBT _    -> Addr "0x0"
| UnknownBT _ -> assert(false) (* should not happen after contract preprocessing *)
| EnumBT _    -> Uint 0
| ContractBT _-> Addr "0x0"

(* initialized the contract storage upon deployment *)
let init_storage (Contract(_,_,vdl,_)) : ide -> exprval =
  List.fold_left (fun acc (vd : var_decl) -> 
    let (x,v) = (match vd.ty with 
      | VarT(t)     -> (vd.name, match vd.init_value with 
          | None -> default_var_value t 
          | Some v -> v)
      | MapT(_,tv)  -> (vd.name, Map (fun _ -> (default_var_value tv)))
    )
    in bind x v acc) 
  botenv 
  vdl 

let init_sysstate = { 
    accounts = (fun a -> failwith ("account " ^ a ^ " unbound")); 
    callstack = [];
    blocknum = 0;
    active = []; 
}

let exec_cmd (n_steps : int) (c : cmd) (st : sysstate) : exec_state =
  let rec exec_rec_cmd n s =
    if n<=0 then s
    else try
        let s' = step_cmd s
        in exec_rec_cmd (n-1) s'
      with NoRuleApplies -> s
    in exec_rec_cmd n_steps (CmdSt (c,st))

let trace_cmd n_steps (c:cmd) (st : sysstate) : exec_state list =
  let rec trace_rec_cmd n t =
    if n<=0 then [t]
    else try
        let t' = step_cmd t
        in t::(trace_rec_cmd (n-1) t')
      with NoRuleApplies -> [t]
  in trace_rec_cmd n_steps (CmdSt(c,st))


(******************************************************************************)
(* Funds an account in a system state. Creates account if it does not exist   *)
(******************************************************************************)

let faucet (a : addr) (n : int) (st : sysstate) : sysstate = 
  if exists_account st a then 
    let as' = { (st.accounts a) with balance = n + (st.accounts a).balance } in
    { st with accounts = bind a as' st.accounts }
  else
    let as' = { balance = n; storage = botenv; code = None; } in
    { st with accounts = bind a as' st.accounts; active = a::st.active }


(******************************************************************************)
(* Executes steps of a transaction in a system state, returning a trace       *)
(******************************************************************************)

let exec_tx (n_steps : int) (tx: transaction) (st : sysstate) : (sysstate,string) result =
  if tx.txvalue < 0 then
    Error ("trying to send a negative amount of tokens")
  else if not (exists_account st tx.txsender) then 
    Error ("sender address " ^ tx.txsender ^ " does not exist")
  else if (st.accounts tx.txsender).balance < tx.txvalue then
    Error ("sender address " ^ tx.txsender ^ " has not sufficient balance")
  else if not (exists_account st tx.txto) && tx.txfun <> "constructor" then
    Error ("to address " ^ tx.txto ^ " does not exist")
  else if (exists_account st tx.txto) && tx.txfun = "constructor" then
    Error ("calling constructor in already deployed contract at address " ^ tx.txto) 
  else try (
    let (sender_state : account_state) = 
      { (st.accounts tx.txsender) with balance = (st.accounts tx.txsender).balance - tx.txvalue } in
    (* updates state of "to" address. If not created yet, deploys the contract *)
    let (to_state : account_state),(deploy : bool) = 
      if exists_account st tx.txto 
      then    { (st.accounts tx.txto) with balance = (st.accounts tx.txto).balance + tx.txvalue }, 
              false (* deploy=false ==> cannot call constructor *) 
      else (match tx.txargs with 
        | Addr(code)::_ ->
            (try let c = code |> parse_contract |> preprocess_contract in 
              { balance=tx.txvalue; storage = init_storage c; code = Some c }, 
              true (* deploy=true ==> must call constructor *)
            with _ -> failwith ("exec_tx: syntax error in contract code: " ^ code))
        | _ -> failwith "exec_tx: the first parameter of a deploy transaction must be the contract code")
    in
    (* executes the called function *)
    match to_state.code with
    | None -> Error "Called address is not a contract"
    | Some src -> (match find_fun_in_contract src tx.txfun with
      | None when (not deploy) -> 
        Error ("Contract at address " ^ tx.txto ^ " has no function named " ^ tx.txfun)
      | None -> (* deploy a contract with no constructor (non-payable) *)
        if tx.txvalue > 0 then 
          Error "The deployed contract should have a payable constructor if you send value"
        else
          Ok { accounts = st.accounts 
              |> bind tx.txsender sender_state
              |> bind tx.txto to_state; 
            callstack = st.callstack;
            blocknum = st.blocknum;
            active = tx.txto :: st.active }
      | Some (Proc(_,xl,c,_,m,_))
      | Some (Constr(xl,c,m)) ->
        if m<>Payable && tx.txvalue>0 then 
            Error "sending ETH to a non-payable function"
        else
          let xl',vl' =
            if deploy then match tx.txargs with 
              _::al -> 
              { ty=VarT(AddrBT false); name="msg.sender"; } ::
              { ty=VarT(UintBT); name="msg.value"; } :: xl
              ,
              Addr tx.txsender :: 
              Uint tx.txvalue :: 
              al
              | _ -> assert(false) (* should never happen *)
            else
              { ty=VarT(AddrBT false); name="msg.sender"; } :: 
              { ty=VarT(UintBT); name="msg.value"; } :: xl,
              Addr tx.txsender :: Uint tx.txvalue :: tx.txargs
          in
          let fr' = { callee = tx.txto; locals = [bind_fargs_aargs xl' vl'] } in
          let st' = { accounts = st.accounts 
                        |> bind tx.txsender sender_state
                        |> bind tx.txto to_state; 
                      callstack = fr' :: st.callstack;
                      blocknum = st.blocknum;
                      active = if deploy then tx.txto :: st.active else st.active } in
          try (match exec_cmd n_steps c st' with
            | St st'' -> Ok (st'' |> pop_callstack)
            | Reverted msg -> Error msg  (* if the command reverts, the new state is st *)
            | _ -> Ok st (* exec_tx: execution of command not terminated (not enough gas?) => revert *)
          )
          with _ as ex -> Error (Printexc.to_string ex) (* exception thrown during execution of command => revert *)
    ) (* match *)
  ) (* try *)
  with _ as ex -> Error (Printexc.to_string ex) 

let exec_tx_list (n_steps : int) (txl : transaction list) (st : sysstate) = 
  List.fold_left 
  (fun sti tx -> match exec_tx n_steps tx sti with 
    | Ok sti' -> sti' 
    | Error _ -> sti) 
  st
  txl

(******************************************************************************)
(*                       Deploys a contract in a system state                 *)
(******************************************************************************)

let deploy_contract (tx : transaction) (src : string) (st : sysstate) : sysstate =
  if exists_account st tx.txto then 
    failwith ("deploy_contract: address " ^ tx.txto ^ " already bound in sysstate")
  else if tx.txfun <> "constructor" then
    failwith ("deploy_contract: deploying a contract must call the constructor")
  else let tx' = { tx with txargs = Addr(src) :: tx.txargs }
  in match exec_tx 1000 tx' st with
    | Ok st' -> st'
    | Error _ -> st
