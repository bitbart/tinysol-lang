(* variable/function/contract identifier *)
type ide = string

(* address identifier *)
type addr = string

(* expressions *)

type expr =
  | True
  | False
  | IntConst of int
  | AddrConst of addr
  | This
  | Var of ide
  | BalanceOf of expr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Leq of expr * expr
  | Le of expr * expr           
  | Geq of expr * expr
  | Ge of expr * expr           
  | IntCast of expr
  | AddrCast of expr

(* commands *)
          
and cmd =
  | Skip
  | Assign of ide * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | Send of expr * expr       (* send(e1,e2) transfers e2 wei to e1 *)
  | Req of expr               (* require(e) reverts if e is false *) 
  | Call of ide * expr list
  | ExecCall of cmd           (* Runtime only: c is the cmd being reduced *)
  | Block of var_decls * cmd
  | ExecBlock of cmd          (* Runtime only: c is the cmd being reduced *)

and var_decl =
  | IntVar of ide 
  | UintVar of ide
  | BoolVar of ide 
  | AddrVar of ide 

and visibility = 
  | Public 
  | Private

and payability = bool 

and fun_decl =
  | Constr of var_decls * cmd * payability
  | Proc of ide * var_decls * cmd * visibility * payability

and var_decls = var_decl list

and fun_decls = fun_decl list

type contract = Contract of ide * var_decls * fun_decls

(* exprval: values associated to (contract and local) variables *)

type exprval = 
  | Bool of bool 
  | Int of int
  | Addr of string

(* in a deploy transaction, the txfun is "constructor" and the first argument is the contract code *)

type transaction = {
  txsender : addr;
  txto : addr;
  txfun : ide;
  txargs : exprval list;
  txvalue : int;
}

(******************************************************************************)
(*                                    Tinysol CLI                             *)
(******************************************************************************)

type cli_cmd = 
  | Faucet of addr * int
  | Deploy of transaction * string
  | CallFun of transaction
  | Revert of transaction
  | Assert of addr * expr
