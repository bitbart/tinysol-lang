(******************************************************************************)
(*                                     Contracts                              *)
(******************************************************************************)

(* variable/function/contract identifier *)
type ide = string

(* address identifier *)
type addr = string

(* Abstract syntax of expressions *)

type expr =
  | BoolConst of bool
  | IntConst of int
  | IntVal of int       (* runtime only: integer expressions *)
  | UintVal of int      (* runtime only: unsigned integer expressions *)
  | AddrConst of addr
  | BlockNum
  | This
  | Var of ide
  | MapR of expr * expr
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
  | Lt of expr * expr           
  | Geq of expr * expr
  | Gt of expr * expr
  | IfE of expr * expr * expr           
  | IntCast of expr
  | UintCast of expr
  | AddrCast of expr
  | PayableCast of expr
  | EnumOpt of ide * ide
  | EnumCast of ide * expr
  | FunCall of expr * ide * expr * expr list
  | ExecFunCall of cmd
  
(* Abstract syntax of commands *)
          
and cmd =
  | Skip
  | Assign of ide * expr                (* Variable assignment *)
  | MapW of ide * expr * expr           (* Map assignment *)
  | Seq of cmd * cmd                    (* Sequencing *)
  | If of expr * cmd * cmd              (* Conditional command *)
  | Send of expr * expr                 (* send(e1,e2) transfers e2 wei to e1 *)
  | Req of expr                         (* require(e) reverts if e is false *) 
  | Block of local_var_decl list * cmd  (* block with declarations *)
  | ExecBlock of cmd                    (* Runtime only: c is the cmd being reduced *)
  | Decl of local_var_decl              (* Static-time only: Decl is converted into block*)
  | ProcCall of expr * ide * expr * expr list
  | ExecProcCall of cmd                 (* Runtime only: c is the cmd being reduced *)
  | Return of expr

(* Base types *)

and base_type = 
  | IntBT           (* int *)
  | UintBT          (* uint *) 
  | BoolBT          (* bool *)
  | AddrBT of bool  (* address (the bool b in AddrBT(b) tells if the address is payable (b=1) or not (b=0) *)
  | CustomBT of ide (* custom type: specialized after preprocessing in EnumBT of ContractBT *)
  | EnumBT of ide

(* Variable types, consisting of:
  - a base type, or
  - a mapping from a base type to another base type
*)

and var_type = VarT of base_type | MapT of base_type * base_type

and visibility_t = 
  | Public 
  | Private
  | Internal
  | External

and mutability_t = 
  | Payable
  | NonPayable
  | View
  | Pure

(* a variable declaration (t,x) consists of:
   - a type t
   - an identifier x of the variable
   - a variable visibility modifier (default is internal)
   - a bool i telling whether the variable is mutable (i=false) or immutable (i=true)
 *)

(* Visibility modifiers *)

and var_decl = { ty: var_type; name: ide; visibility: visibility_t; immutable: bool }

and local_var_decl = { ty: var_type; name: ide; }

(* Function declarations
  - the constructor is always public, and it can be payable
  - functions can be either public or private, and they can be payable
 *)

type fun_decl =
  | Constr of local_var_decl list * cmd * mutability_t
  | Proc of ide * local_var_decl list * cmd * visibility_t * mutability_t * (base_type option) 

type enum_decl = Enum of (ide * ide list)

(* Contracts consist of:
 - a name (ide)
 - a list of enum declarations 
 - a list of variable declarations (contract state variables)
 - a list of function declarations 
 *)
type contract = Contract of ide * enum_decl list * var_decl list * fun_decl list


(******************************************************************************)
(*                                   Transactions                             *)
(******************************************************************************)

(* Exprval: values associated to (contract and local) variables *)

type exprval = 
  | Bool of bool 
  | Int of int
  | Uint of int
  | Addr of string
  | Map of (exprval -> exprval)

(* Transactions contain:
 - txsender: the address of the transaction sender (either an EOA or a contract address)
 - txto: the address of the called contract
 - txfun: the name of the called function. In a deploy transaction, the txfun is "constructor" and the first argument is the contract code
 - txvars: the list of actual parameters
 - txvalue: the amount of wei transferred from the sender to the caller along with the transaction 
 *)

type transaction = {
  txsender : addr;
  txto : addr;
  txfun : ide;
  txargs : exprval list;
  txvalue : int;
}
