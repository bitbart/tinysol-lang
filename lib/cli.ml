(******************************************************************************)
(*                                    Tinysol CLI                             *)
(******************************************************************************)

open Ast
open Cli_ast
open Utils
open Sysstate
open Main
open Prettyprint

let string_of_cli_cmd = function 
  | Faucet(a,n) -> "faucet " ^ a ^ " " ^ string_of_int n
  | Deploy(tx,filename) -> "deploy " ^ string_of_transaction tx ^ " " ^ filename
  | CallFun tx -> string_of_transaction tx
  | Revert tx -> "revert " ^ string_of_transaction tx
  | Assert(a,e) -> "assert " ^ a ^ " " ^ string_of_expr e
  | LastReverted -> "lastReverted"
  | NotLastReverted -> "!lastReverted" 
  | SetBlockNum(n) -> "block.number = " ^ string_of_int n

let is_empty_or_comment (s : string) =
  let len = String.length s in
  (* skip leading spaces *)
  let rec skip i =
    if i >= len then true                      (* string is only spaces → empty *)
    else if s.[i] = ' ' then skip (i + 1)
    else if i + 1 < len && s.[i] = '/' && s.[i+1] = '/' then true
    else false
  in
  skip 0

let is_assert = function 
  | Assert _ 
  | LastReverted 
  | NotLastReverted -> true
  | _ -> false

let exec_cli_cmd (cc : cli_cmd) (lastReverted : bool) (st : sysstate) : sysstate = match cc with
  | Faucet(a,n) -> faucet a n st
  | Deploy(tx,filename) -> 
      let src = filename |> read_file 
      in st |> deploy_contract tx src
  | CallFun tx -> st |> exec_tx 1000 tx |> fun res -> (match res with
      | Ok st -> st
      | Error msg -> failwith msg)
  | Revert tx -> 
      st |> exec_tx 1000 tx |> fun res -> (match res with  
        | Ok _ -> failwith ("revert violation: transaction " ^ string_of_transaction tx ^ " should revert") 
        | Error _ -> st)
  | Assert(a,e) -> 
    (match eval_expr { st with callstack = [{callee = a; locals = []}] } e with
    | Bool true -> st
    | _ -> failwith ("assertion violation: " ^ string_of_cli_cmd cc)) 
  | LastReverted -> if lastReverted then st else failwith "lastReverted" 
  | NotLastReverted -> if (not lastReverted) then st else failwith "!lastReverted"
  | SetBlockNum(n) -> { st with blocknum = n }

let exec_cli_cmd_list (verbose : bool) (ccl : cli_cmd list) (st : sysstate) = 
  List.fold_left 
  (fun (lastReverted,sti) cc -> 
    if verbose && not (is_assert cc) then 
      print_endline (string_of_sysstate [] sti ^ "\n--- " ^ string_of_cli_cmd cc ^ " --->")
    else ();  
    try 
      (false, exec_cli_cmd cc lastReverted sti)
    with ex -> 
      let _ = print_endline (Printexc.to_string ex) in
      (true, sti)
  )
  (false, st)
  ccl
