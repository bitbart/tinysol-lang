open Ast
open Types

let apply st x = match topenv st x with
    IVar l -> getmem st l
  | _ -> failwith "apply error"

let parse_cmd (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.cmd_test Lexer.read_token lexbuf in
  ast

let parse_contract (s : string) : contract =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.contract Lexer.read_token lexbuf in
  ast
  
exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies


let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botmem = fun l -> failwith ("location " ^ string_of_int l ^ " undefined")
    
let bind f x v = fun y -> if y=x then v else f y

(******************************************************************************)
(*                       Big-step semantics of expressions                    *)
(******************************************************************************)

let is_val = function
    True -> true
  | False -> true
  | IntConst _ -> true
  | AddrConst _ -> true
  | _ -> false

let rec eval_expr st = function
    True -> Bool true
  | False -> Bool false
  | Var x -> Int (apply st x)
  | IntConst n -> Int n
  | AddrConst s -> Addr s
  | Not(e) -> (match eval_expr st e with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not")
    )
  | And(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 && b2)
      | _ -> raise (TypeError "And")
    )
  | Or(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 || b2)
      | _ -> raise (TypeError "Or")
    )
  | Add(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Int(n1 + n2)
      | _ -> raise (TypeError "Add")
    )    
  | Sub(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) when n1>=n2 -> Int(n1 - n2)
      | _ -> raise (TypeError "Sub")
    )
  | Mul(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Int(n1 * n2)
      | _ -> raise (TypeError "Add")
    )        
  | Eq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 = n2)
      | _ -> raise (TypeError "Eq")
    )    
  | Leq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 <= n2)
      | _ -> raise (TypeError "Leq")
    )          
  | Le(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 < n2)
      | _ -> raise (TypeError "Le")
    )          
  | Geq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 >= n2)
      | _ -> raise (TypeError "Geq")
    )          
  | Ge(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 > n2)
      | _ -> raise (TypeError "Ge")
    )          


(******************************************************************************)
(*                       Small-step semantics of commands                     *)
(******************************************************************************)

let rec trace1_cmd = function
    St _ -> raise NoRuleApplies
  | Cmd(c,st) -> match c with
      Skip -> St st
    | Assign(x,e) -> (match (topenv st x,eval_expr st e) with
          (IVar l,Int n) -> St (getenv st, bind (getmem st) l n, getloc st)
        | _ -> failwith "assign: type error")
    | Seq(c1,c2) -> (match trace1_cmd (Cmd(c1,st)) with
          St st1 -> Cmd(c2,st1)
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))
    | If(e,c1,c2) -> (match eval_expr st e with
          Bool true -> Cmd(c1,st)
        | Bool false -> Cmd(c2,st)
        | _ -> failwith("if: type error"))
    | Req(_) -> failwith ("TODO")
    | Send(_,_,_) -> failwith ("TODO")
    | Call(_,_) -> failwith "TODO"
(* (match (topenv st f,eval_expr st e) with
          (IProc(a,c),Int n) ->
          let l = getloc st in
          let env' = bind (topenv st) x (IVar l) in
          let mem' = bind (getmem st) l n in
          let st' = (env'::(getenv st), mem', l+1) in
          Cmd(CallExec(c),st')
        | _ -> raise (TypeError "Call of a non-procedure"))
*)
    | CallExec(c) -> (match trace1_cmd (Cmd(c,st)) with
          St st' -> St (popenv st', getmem st', getloc st')
        | Cmd(c',st') -> Cmd(CallExec(c'),st'))


let sem_decl (e,l) = function
  | IntVar(x) ->  let e' = bind e x (IVar l) in (e',l+1)
  | Constr(f,a,c) -> let e' = bind e f (IProc(a,c)) in (e',l)                                                
  | Proc(f,a,c) -> let e' = bind e f (IProc(a,c)) in (e',l)


let rec trace_rec_cmd n t =
  if n<=0 then [t]
  else try
      let t' = trace1_cmd t
      in t::(trace_rec_cmd (n-1) t')
    with NoRuleApplies -> [t]


let trace_cmd n_steps c =
  trace_rec_cmd n_steps (Cmd(c,([botenv],botmem,0)))


(******************************************************************************)
(*                       Small-step semantics of transactions                 *)
(******************************************************************************)

let trace_tx n_steps (Tx(a,c,f,args)) (BCState bs)
    = 

(**********************************************************************
 trace : int -> contract -> conf list

 Usage: trace n c performs n steps of the small-step semantics
 **********************************************************************)

(*
let trace n (Contract(d)) =
  let (e,l) = sem_decl (botenv,0) d
  in trace_rec n (Cmd(c,([e],botmem,l)))
*)

(*
let trace n t = trace_rec n (Cmd(t,bot))
*)



let trace_tx n_steps c =
  trace_rec_cmd n_steps (Cmd(c,([botenv],botmem,0)))
