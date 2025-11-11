open TinysolLib.Prettyprint
open TinysolLib.Main
       
(* read file, and output it to a string *)

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s

(* read line from standard input, and output it to a string *)

let read_line () =
  try Some(read_line())
  with End_of_file -> None
;;

match Array.length(Sys.argv) with
(* trace n / read input from stdin *) 
  1 -> (match read_line() with
      Some s when s<>"" -> s |> parse_cmd |> string_of_cmd |> print_string
    | _ -> print_newline())
(* trace1 / read input from file *) 
| 2 -> (match read_file Sys.argv.(1) with
      "" -> print_newline()
    | s -> s |> parse_contract |> string_of_contract |> print_string)
(* wrong usage *)      
| _ -> failwith "Usage: dune exec tinysol-lang n_steps [file]"
