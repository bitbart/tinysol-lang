open TinysolLib.Main
open TinysolLib.Sysstate
open TinysolLib.Utils
open TinysolLib.Cli
open TinysolLib.Prettyprint
open TinysolLib.Typechecker
;;

match Array.length(Sys.argv) with
(* parse_cmd *) 
  2 when Sys.argv.(1)="parse_cmd" -> (match read_line() with
      Some s when s<>"" -> s |> parse_cmd |> string_of_cmd |> print_string
    | _ -> print_newline())
(* exec_cmd *)
| 3 when Sys.argv.(1)="exec_cmd" -> (match read_line() with
    | Some s when s<>"" -> s |> parse_cmd |> blockify_cmd (* TODO: enumify? *)
      |> fun c -> trace_cmd (int_of_string Sys.argv.(2)) c 
      (push_callstack {callee="0xC"; locals=[];} init_sysstate)
      |> string_of_trace |> print_string
    | _ -> print_newline())
(* parse_contract *) 
| 2 when Sys.argv.(1)="parse" -> (match read_line() with
    | Some s when s<>"" -> s |> parse_contract |> preprocess_contract |> string_of_contract |> print_string
    | _ -> print_newline())
| 3 when Sys.argv.(1)="parse" -> (match read_file Sys.argv.(2) with
      "" -> print_newline()
    | s -> s |> parse_contract |> preprocess_contract |> string_of_contract |> print_string)
(* typecheck contract*)
| 2 when Sys.argv.(1)="typecheck" -> (match read_line() with
    | Some s when s<>"" -> s |> parse_contract |> preprocess_contract |> typecheck_contract 
             |> string_of_typecheck_result |> print_endline 
    | _ -> print_newline())
| 3 when Sys.argv.(1)="typecheck" -> (match read_file Sys.argv.(2) with
      "" -> print_newline()
    | s -> s |> parse_contract |> preprocess_contract |> typecheck_contract 
             |> string_of_typecheck_result |> print_endline) 
(* unittest *)
| 3 when Sys.argv.(1)="unittest" ->
  Sys.argv.(2) |> read_lines 
  |> List.filter (fun s -> not (is_empty_or_comment s)) 
  |> List.map parse_cli_cmd 
  |> fun l -> snd (exec_cli_cmd_list true l (init_sysstate))
  |> string_of_sysstate [] |> print_string
(* wrong usage *)      
| _ -> print_string "Usage:
  dune exec tinysol parse_cmd         : parses cmd in stdin
  dune exec tinysol exec_cmd <n>      : executes n_steps of cmd in stdin
  dune exec tinysol parse [<file>]    : parses contract in file or stdin
  dune exec tinysol typecheck [<file>]: typechecks contract in file or stdin
  dune exec tinysol unittest <file>   : executes CLI commands from file 
"
