open TinysolLib.Types       
open TinysolLib.Prettyprint
open TinysolLib.Main


(**********************************************************************
 trace test : (command, n_steps, location, expected value after n_steps)
 **********************************************************************)

let test_parse_cmd (cmd,t) =
  cmd
  |> parse_cmd
  |> fun x -> x = t

let test_trace_cmd (cmd,n_steps,var,exp_val) =
  cmd
  |> parse_cmd
  |> fun c -> last (trace_cmd n_steps c)
  |> fun t -> match t with
    St s -> apply s var = exp_val
  | Cmd(_,_) -> failwith "program not terminated"

let%test "test_parse_cmd_1" = test_parse_cmd
  ("skip", Skip)  

let%test "test_parse_cmd_2" = test_parse_cmd
  ("x=51", Assign("x",IntConst 51))  

let%test "test_trace_cmd_1" = test_trace_cmd
  ("x=51", 1, "x", 51)  
