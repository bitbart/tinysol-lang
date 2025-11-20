open TinysolLib.Ast
open TinysolLib.Types       
open TinysolLib.Utils
open TinysolLib.Main
open TinysolLib.Static


(********************************************************************************
 test_parse_cmd : (command, expected AST)
 ********************************************************************************)

let test_parse_cmd (c : string) (t : cmd) =
  c
  |> parse_cmd
  |> fun x -> x = t

let%test "test_parse_cmd_1" = test_parse_cmd
  "skip;" 
  Skip  

let%test "test_parse_cmd_2" = test_parse_cmd
  "x=51;"
  (Assign("x",IntConst 51))  

let%test "test_parse_cmd_3" = test_parse_cmd
  "{ int x; x=51; }" 
  (Block([IntVar "x"],Assign("x",IntConst 51)))  

let%test "test_parse_cmd_4" = test_parse_cmd
  "{ int x; x=51; x=x+1; }"
  (Block([IntVar "x"],Seq(Assign("x",IntConst 51),Assign("x",Add(Var "x",IntConst 1)))))  

let%test "test_parse_cmd_5" = test_parse_cmd
  "{ int x; x=51; x=x+1; skip; }" 
  (Block([IntVar "x"],
    Seq(
      Assign("x",IntConst 51),
      Seq(
        Assign("x",Add(Var "x",IntConst 1)),
        Skip
      )
    )))


(********************************************************************************
 test_trace_cmd : (command, n_steps, variable, expected value after n_steps)
 ********************************************************************************)

let test_trace_cmd (c,n_steps,var,exp_val) =
  c
  |> parse_cmd
  |> fun c -> last (trace_cmd n_steps c "0xCAFE" init_sysstate)
  |> fun t -> match t with
  | St st -> lookup_var "0xCAFE" var st = exp_val
  | Cmd(_,st,_) -> lookup_var "0xCAFE" var st = exp_val
  | Reverted -> false

let%test "test_trace_cmd_1" = test_trace_cmd
  ("{ int x; x=51; skip; }", 2, "x", Int 51)  

let%test "test_trace_cmd_2" = test_trace_cmd
  ("{ int x; x=51; x=x+1; skip; }", 3, "x", Int 52)  

let%test "test_trace_cmd_3" = test_trace_cmd
  ("{ int x; x=51; { int x; x=42; } x=x+1; skip; }", 5, "x", Int 52)  

let%test "test_trace_cmd_4" = test_trace_cmd
  ("{ int x; x=51; { int x; x=42; skip; skip; skip; } x=x+1; skip; }", 5, "x", Int 42)  

let%test "test_trace_cmd_5" = test_trace_cmd
  ("{ int x; x=51; { int x; x=x+1; skip; skip; skip; } x=x+1; skip; }", 5, "x", Int 1)  

let%test "test_trace_cmd_6" = test_trace_cmd
  ("{ int x; x=51; { int x; x=1; } { int x; x=x+3; } x=x+5; skip; }", 7, "x", Int 56)  

let%test "test_trace_cmd_7" = test_trace_cmd
  ("{ int x; x=51; { int y; y=x+1; skip; } { x = 0; } x=x+5; skip; }", 7, "x", Int 5)  

let%test "test_trace_cmd_8" = test_trace_cmd
  ("{ int x; x=51; { int y; y=x+1; skip; } { int x; x = 0; } x=x+5; skip; }", 8, "x", Int 56)  

let%test "test_trace_cmd_9" = test_trace_cmd
  ("{ bool b; b = 2==2; skip; }", 2, "b", Bool true)  

let%test "test_trace_cmd_10" = test_trace_cmd
  ("{ int x; if (x==0) x=1; else x=2; skip; }", 3, "x", Int 1)  

let%test "test_trace_cmd_11" = test_trace_cmd
  ("{ int x; if (x>0) x=1; else x=2; skip; }", 3, "x", Int 2)  

let%test "test_trace_cmd_12" = test_trace_cmd
  ("{ int x; bool b; if (b) x=2; else b=true; skip; }", 2, "x", Int 0)  




(********************************************************************************
 test_exec_tx : 
 - c: the contract that will be deployed for testing,
 - txl: a list of transactions that will be executed
 - vars: a list of state variables of contract c that will be inspected
 - exp_vals: a list of expected values for the variables vars)
 ********************************************************************************)

let test_exec_tx (src: string) (txl: string list) (els : string list) =
  let txl = List.map parse_transaction txl in
  init_sysstate
  |> faucet "0xA" 100
  |> faucet "0xB" 100
  |> deploy_contract { txsender="0xA"; txto="0xC"; txfun="constructor"; txargs=[]; txvalue=0; } src 
  |> exec_tx_list 1000 txl 
  |> fun st -> List.map (fun x -> x |> parse_expr |> eval_expr st "0xC") els 
  |> List.for_all (fun v -> v = Bool true)

let c1 = "contract C {
    int x;
    bool b;
  
    function g() public { 
        if (b) x = x+1;
        else b=true;
    }
}"

let%test "test_exec_tx_1" = test_exec_tx
  c1
  ["0xA:0xC.g()"] 
  ["x==0"; "b"]

let%test "test_exec_tx_2" = test_exec_tx
  c1
  ["0xA:0xC.g()"; "0xA:0xC.g()"] 
  ["x==1"; "b"]

let%test "test_exec_tx_3" = test_exec_tx
  c1
  ["0xA:0xC.g()"; "0xA:0xC.g()"; "0xA:0xC.g()"] 
  ["x==2"; "b"]


let c2 = "contract C {
    int x;
    address owner;
  
    constructor() { owner = msg.sender; }

    function f() public { 
        require(msg.sender == owner);
        x = x+1;
    }
}"

let%test "test_exec_tx_4" = test_exec_tx
  c2
  ["0xA:0xC.f()"] 
  ["x==1"; "owner==\"0xA\""]  

let%test "test_exec_tx_5" = test_exec_tx
  c2
  ["0xA:0xC.f()"; "0xB:0xC.f()"; "0xA:0xC.f()"] 
  ["x==2"; "owner==\"0xA\""]

let c3 = "contract C {
    int x;
  
    function f() public payable { 
        require(msg.value > 0);
        x = x+1;
    }
}"

let%test "test_exec_tx_6" = test_exec_tx
  c3
  ["0xA:0xC.f()"] 
  ["x==0"; "this.balance==0"]

let%test "test_exec_tx_7" = test_exec_tx
  c3
  ["0xA:0xC.f{value : 3}()"] 
  ["x==1"; "this.balance==3"]


let c4 = "contract C {
    int x;
  
    function f() public payable { 
        x = x+1;
        require(x > 0 && msg.value > 0);
    }
}"

let%test "test_exec_tx_7" = test_exec_tx
  c4
  ["0xA:0xC.f()"] 
  ["x==0"; "this.balance==0"]

let%test "test_exec_tx_8" = test_exec_tx
  c4
  ["0xA:0xC.f{value : 3}()"] 
  ["x==1"; "this.balance==3"]


let test_typecheck (src: string) (exp : bool)=
  let c = parse_contract src in typecheck_contract c = exp  

let%test "test_typecheck_0" = test_typecheck 
  "contract C0 { }"
  true

let%test "test_typecheck_1" = test_typecheck c1 true

let%test "test_typecheck_2" = test_typecheck c2 true

let%test "test_typecheck_3" = test_typecheck c3 true

let%test "test_typecheck_4" = test_typecheck c4 true

let%test "test_typecheck_5" = test_typecheck 
"contract C {
    int x;
    function f(bool b) public { x = b; }
}"
false

let%test "test_typecheck_6" = test_typecheck 
"contract C {
    int x;
    function f(bool x) public { x = true; }
}"
true

let%test "test_typecheck_7" = test_typecheck 
"contract C {
    int x;
    function f(bool x) public { x = 1; }
}"
false