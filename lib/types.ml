open Ast
    
type loc = int

type exprval = Bool of bool | Int of int | Addr of string
type envval = IVar of loc | IProc of args * cmd
type memval = int

type env = ide -> envval
type mem = loc -> memval

(* contract transient state  
   The third component of the state is the first free location.
   We assume that the store is unbounded *)

type state = env list * mem * loc

let topenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

let popenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

let getenv (el,_,_) = el
let getmem (_,m,_) = m
let getloc (_,_,l) = l

(* cmd configuration *)
type conf = St of state | Cmd of cmd * state

exception TypeError of string
exception UnboundVar of ide

(* wallet state = mapping from tokens to amounts *)
type wstate = tok -> int

(* contract persistent state =   *)
type cstate = env * wstate
  
type addrstate = WAddr of wstate | CAddr of cstate
type bcstate = ide -> addrstate
