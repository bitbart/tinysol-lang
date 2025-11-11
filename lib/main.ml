open Ast
open Types

let apply st x = match topenv st x with
    IVar l -> getmem st l
  | _ -> failwith "apply error"

let parse (s : string) : contract =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.contract Lexer.read_token lexbuf in
  ast
  
(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies

let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botmem = fun l -> failwith ("location " ^ string_of_int l ^ " undefined")
    
let bind f x v = fun y -> if y=x then v else f y

let is_val = function
    True -> true
  | False -> true
  | IntConst _ -> true
  | StringConst _ -> true
  | _ -> false


(******************************************************************************)
(*                       Big-step semantics of expressions                    *)
(******************************************************************************)

let rec eval_expr st = function
    True -> Bool true
  | False -> Bool false
  | Var x -> Int (apply st x)
  | IntConst n -> Int n
  | StringConst s -> String s
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

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]

(**********************************************************************
 trace : int -> contract -> conf list

 Usage: trace n c performs n steps of the small-step semantics
 **********************************************************************)

(* 
let trace n (Contract(d)) =
  let (e,l) = sem_decl (botenv,0) d
  in trace_rec n (Cmd(c,([e],botmem,l)))
*)


(******************************************************************************)
(*                                  Encoding in Z3                            *)
(******************************************************************************)

let py_preamble = "
from z3 import *
import time

# N = upper bound on the length of trace
N = 5

# A = upper bound on the number of actors (A+1)
A = 2

# Contract balance
w = [Int(\"w_%s\" % (i)) for i in range(N+1)]
w_q = Int(\"wq\")

# users' wallets
aw = [[Int(\"aw_%s_%s\" % (i, j)) for j in range(A+1)] for i in range(N+1)]
aw_q = [Int(\"awq_%s\" %j) for j in range(A+1)]

# msg.sender
xa = [Int(\"xa_%s\" % (i)) for i in range(N+1)]
xa_q = Int(\"xa_q\")

# msg.value
xn = [Int(\"xn_%s\" % (i)) for i in range(N+1)]
xn_q = Int(\"xn_q\")

s = Solver()

# initial state
s.add(w[0] >= 0)
"

let get_proc_names d =
  List.map (fun p -> match p with Proc(f,_,_) -> f | _ -> assert(false))
    (List.filter (fun x -> match x with Proc(_) -> true | _ -> false) d)  

let py_proc procnames = "Proc = Datatype('Proc')\n" ^ 
                        (List.fold_left (fun s p -> s ^ "\n" ^ "Proc.declare('" ^ p ^ "')") "" procnames) ^ "\n" ^
                        "Proc = Proc.create()\n" ^
                        "# Called functions\n" ^ 
                        "f = [Const(\"f_%s\" % (i), Proc) for i in range(N+1)]\n" ^
                        "f_q = Const(\"f_q\", Proc)\n"

let py_clauses = "
def valid_tx(xa1, xn1, w1 , w2):
    return And(xa1 >= 0, xa1 <= A, xn1 > 0)

def can_pay(xa1, xn1, w1 , w2):
    return And(xn1 <= w1)


def pay_success(xa1, xn1, aw1 , aw2, w1 , w2):
    return And(If(xa1 == 0,
               And(w2 == w1 - (xn1 - 1),
                   And([If(xa1 == j,
                           aw2[j] == aw1[j] + (xn1 - 1),
                           aw2[j] == aw1[j]) for j in range(A+1)])),
               And(w2 == w1 - xn1,
                   And([If(xa1 == j,
                           aw2[j] == aw1[j] + xn1,
                           aw2[j] == aw1[j]) for j in range(A+1)])),
                  ))


def pay_failure(xa1, xn1, aw1, aw2, w1, w2):
    return And(w2 == w1,
               And([aw2[j] == aw1[j] for j in range(A+1)]))


def deposit(xn1, w1, w2, aw1, aw2):
    return And(w2 == w1 + xn1,
               And([aw2[j] == aw1[j] for j in range(A+1)]))

def user_is_legit(xa1):
    return And(xa1 >= 0, xa1 <= A)

def step_trans(f1, xa1, xn1, aw1, aw2, w1, w2, i):
    return And(valid_tx(xa1, xn1, w1, w2),
               And([aw1[j] >= 0 for j in range(A+1)]),
               If(f1 == Proc.pay,
               If(can_pay(xa1, xn1, w1, w2,),
                  pay_success(xa1, xn1, aw1, aw2, w1, w2),
                  pay_failure(xa1, xn1, aw1, aw2, w1, w2)),
               # The only other possible transition is f[i] == Proc.deposit
                  deposit(xn1, w1, w2, aw1, aw2)
                  ))

for i in range(N):
    new_state = step_trans(f[i], xa[i], xn[i], aw[i], aw[i+1], w[i], w[i+1], i) 
    s.add(new_state)
"
