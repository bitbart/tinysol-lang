type ide = string

type tok = string

type expr =
  | True
  | False
  | Var of ide
  | IntConst of int
  | StringConst of string
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
  | Le of expr * expr           
  | Geq of expr * expr
  | Ge of expr * expr           
              
and cmd =
  | Skip
  | Assign of string * expr
  | Seq of cmd * cmd
  | Send of ide * expr * tok
  | If of expr * cmd * cmd
  | Req of expr             
  | Call of ide * expr     
  | CallExec of cmd         (* Runtime only: c is the cmd being reduced *)

and arg =
  | IntArg of ide
  | RcvArg of ide * tok

and args = arg list
    
type decl =
  | IntVar of ide 
  | Constr of ide * args * cmd
  | Proc of ide * args * cmd            

and decls = decl list
    
type contract = Contract of ide * decls
