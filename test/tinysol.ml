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
  (Block([VarT(IntBT,false),"x"],Assign("x",IntConst 51)))  

let%test "test_parse_cmd_4" = test_parse_cmd
  "{ int x; x=51; x=x+1; }"
  (Block([VarT(IntBT,false),"x"],Seq(Assign("x",IntConst 51),Assign("x",Add(Var "x",IntConst 1)))))  

let%test "test_parse_cmd_5" = test_parse_cmd
  "{ int x; x=51; x=x+1; skip; }" 
  (Block([VarT(IntBT,false),"x"],
    Seq(
      Assign("x",IntConst 51),
      Seq(
        Assign("x",Add(Var "x",IntConst 1)),
        Skip
      )
    )))

let%test "test_parse_cmd_6" = test_parse_cmd
  "{ uint x; mapping (uint => int) m; x = m[x+1]; }"
  (Block ([(VarT(UintBT,false), "x"); (MapT (UintBT, IntBT), "m")],
  Assign ("x", MapR (Var "m", Add (Var "x", IntConst 1)))))

let%test "test_parse_cmd_7" = test_parse_cmd
  "{ mapping (uint => uint) m; m[0] = m[0]+1; }"
  (Block ([(MapT (UintBT, UintBT), "m")],
 MapW ("m", IntConst 0, Add (MapR (Var "m", IntConst 0), IntConst 1))))

let%test "test_parse_cmd_8" = test_parse_cmd
  "{ mapping (uint => uint) m; m[m[0]] = m[m[1]+2]+3; }"
  (Block ([(MapT (UintBT, UintBT), "m")],
 MapW ("m", MapR (Var "m", IntConst 0),
  Add (MapR (Var "m", Add (MapR (Var "m", IntConst 1), IntConst 2)),
   IntConst 3))))

  
let%test "test_parse_contract_1" = try 
  let _ = parse_contract
    "contract C {
        function f(mapping (uint => uint) m) public { m[0] = 1; }
    }"
  in false 
  with _ -> true

let%test "test_parse_contract_2" = try
  parse_contract
  "contract C { uint x; function f() public { x = block.number; } }"
  = 
  (Contract ("C", [(VarT(UintBT,false), "x")],
  [Proc ("f", [], Assign ("x", BlockNum), Public, false)]))
  with _ -> false

let%test "test_parse_contract_3" = try 
  let _ = parse_contract
    "contract C {
        int immutable x;
        function f(int y) public { y=x; }
    }"
  in true 
  with _ -> false

let%test "test_parse_contract_4" = try 
  let _ = parse_contract
    "contract C {
        int x;
        function f(int immutable y) public { x=y; }
    }"
  in false 
  with _ -> true

let%test "test_parse_contract_5" = try 
  parse_contract
  "contract C { address payable a; function f() public payable { a.transfer(address(this).balance); } }"
  =
  (Contract ("C", [(VarT (AddrBT true, false), "a")],
  [Proc ("f", [], Send (Var "a", BalanceOf (AddrCast This)), Public, true)]))
  with _ -> false

let%test "test_parse_contract_6" = try 
  parse_contract
  "contract C { function f(address payable a) public payable { a.transfer(address(this).balance); } }"
  =
  (Contract ("C", [],
 [Proc ("f", [(VarT (AddrBT true, false), "a")],
   Send (Var "a", BalanceOf (AddrCast This)), Public, true)]))
  with _ -> false


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


let%test "test_exec_tx_8" = test_exec_tx
  "contract C {
      int x;
      int y;
      function f(int x) public { int x; y=x; x=3; }
      function g(int y) public { x=y; y=-1; }
  }"
  ["0xA:0xC.g(1)"; "0xA:0xC.f(2)"] 
  ["x==1"; "y==0"]

let%test "test_exec_tx_9" = test_exec_tx
  "contract C {
      uint x;
      function f(int y) public { x=7; x = uint(y)-1; }
  }"
  ["0xA:0xC.f(3)"; "0xA:0xC.f(0)"] 
  ["x==2"]


let%test "test_map_1" = test_exec_tx
  "contract C {
      mapping (uint => uint) m;
      uint x;
      function f(int k, int v) public { m[k] = v; }
      function g(int k) public { x = m[k]; }
  }"
  ["0xA:0xC.f(0,1)"; "0xA:0xC.g(0)"] 
  ["x==1"]



let test_typecheck (src: string) (exp : bool)=
  let c = parse_contract src in 
  try typecheck_contract c = exp
  with _ -> not exp  

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

let%test "test_typecheck_8" = test_typecheck 
"contract C {
    int x;
    function f() public { x = x+1; }
}"
true

let%test "test_typecheck_9" = test_typecheck 
"contract C {
    int x;
    function f() public { x = x-1; }
}"
true

let%test "test_typecheck_10" = test_typecheck 
"contract C {
    uint x;
    function f() public { x = x-1; }
}"
true

let%test "test_typecheck_11" = test_typecheck 
"contract C {
    address x;
    function f() public { x = x-1; }
}"
false

let%test "test_typecheck_12" = test_typecheck 
"contract C {
    int x;
    function f() public { x = -1; }
}"
true

let%test "test_typecheck_13" = test_typecheck 
"contract C {
    uint x;
    function f() public { x = -1; }
}"
false

let%test "test_typecheck_14" = test_typecheck 
"contract C {
    uint x;
    function f() public { x = 2-1; }
}"
false

let%test "test_typecheck_15" = test_typecheck 
"contract C {
    uint x;
    function f() public { x = 2+1; }
}"
true

let%test "test_typecheck_16" = test_typecheck 
"contract C {
    uint x;
    function f(int x) public { x = -1; }
}"
true

let%test "test_typecheck_17" = test_typecheck 
"contract C {
    uint x;
    function f(int x) public { x = -1; { bool x; x = true; } x = x+1; }
}"
true

let%test "test_typecheck_18" = test_typecheck 
"contract C {
    int x;
    function f(uint x) public { x = -1; }
}"
false

let%test "test_typecheck_19" = test_typecheck 
"contract C {
    int x; bool b;
    function f(uint x) public { x = b; }
}"
false

let%test "test_typecheck_20" = test_typecheck 
"contract C {
    int x; bool b;
    function f(uint x) public { b = x; }
}"
false

let%test "test_typecheck_21" = test_typecheck 
"contract C {
    int x; address a;
    function f(uint x) public { x = a; }
}"
false

let%test "test_typecheck_22" = test_typecheck 
"contract C {
    int x; address a;
    function f(uint x) public { a = x; }
}"
false

let%test "test_typecheck_23" = test_typecheck 
"contract C {
    int x;
    function f(uint y) public { y = x; }
}"
false

let%test "test_typecheck_24" = test_typecheck 
"contract C {
    int x;
    // uint is not convertible to int
    function f(uint y) public { x = y; }
}"
false

let%test "test_typecheck_25" = test_typecheck 
"contract C {
    int x;
    // uint is not comparable to int
    function f(uint y) public { require (x < y); }
}"
false

let%test "test_typecheck_26" = test_typecheck 
"contract C {
    uint x;
    // uint is not comparable to int
    function f(int y) public { require (x < y); }
}"
false

let%test "test_typecheck_27" = test_typecheck 
"contract C {
    int x;
    function f(int y) public { require (x < y && y < -5); }
}"
true

let%test "test_typecheck_28" = test_typecheck 
"contract C {
    uint x;
    function f(uint y) public { require (x < y && y < 5); }
}"
true

let%test "test_typecheck_29" = test_typecheck 
"contract C {
    uint x;
    function f(uint y) public { require (x < y && y == x+5); }
}"
true

let%test "test_typecheck_29" = test_typecheck 
"contract C {
    int x;
    function f(int y) public { require (x < y && y != 5); }
}"
true

let%test "test_typecheck_30" = test_typecheck 
"contract C {
    int x;
    function f(uint y) public { require uint(x) < y; x = int(y); }
}"
true

let%test "test_typecheck_31" = test_typecheck 
"contract C {
    int x;
    function f(uint y) public { require int(y) < x; y = uint(x); }
}"
true

let%test "test_typecheck_32" = test_typecheck 
"contract C {
    int x;
    function f(uint y) public { require int(y)+x < 7; y = uint(x) + 1; }
}"
true

let%test "test_typecheck_33" = test_typecheck 
"contract C {
    int x;
    int x;
    function f(int y) public { require x+y < 7; }
}"
false

let%test "test_typecheck_34" = test_typecheck 
"contract C {
    int x;
    function f(int y) public { require x+y < 7; }
    function g(bool b) public { require b; }
    function h(address a) public { }
    function f(bool b) public { require b; }
}"
false

let%test "test_typecheck_35" = test_typecheck 
"contract C {
    int x;
    function f(int y) public { require x+y < 7; }
    function g(bool b) public { require b; }
    function h(address a) public { }
}"
true

let%test "test_typecheck_36" = test_typecheck 
"contract C {
    int x;
    constructor(int y) { x=y; }
    function g(bool b) public { require b; }
    constructor() { x=1; }
}"
false

let%test "test_typecheck_37" = test_typecheck 
"contract C {
    int x;
    function f(int b, address a, bool b) public { require b; }
}"
false

let%test "test_typecheck_38" = test_typecheck 
"contract C {
    int x;
    function f() public { { int y; y=x+1; } }
}"
true

let%test "test_typecheck_39" = test_typecheck 
"contract C {
    int x;
    function f(int x) public { }
}"
true

let%test "test_typecheck_40" = test_typecheck 
"contract C {
    int x;
    function f(int x) public { int x; x = 1; }
}"
true

let%test "test_typecheck_41" = test_typecheck 
  "contract C {
      uint x;
      function f(int y) public { x=7; x = uint(y)-1; }
  }"
  true

let%test "test_typecheck_mapping_1" = test_typecheck 
  "contract C {
      mapping (uint => uint) m;
      uint x;
      function w(uint k, uint v) public { m[k] = v; }
      function r(uint k) public { x = m[k]; }
  }"
  true

let%test "test_typecheck_mapping_2" = test_typecheck 
  "contract C {
      mapping (uint => uint) m;
      uint x;
      function w(uint k, uint v) public { x[k] = v; }
      function r(uint k) public { x = m[k]; }
  }"
  false

let%test "test_typecheck_mapping_3" = test_typecheck 
  "contract C {
      mapping (uint => uint) m;
      uint x;
      function w(uint k, uint v) public { m[k] = v; }
      function r(uint k) public { x[k] = m[k]; }
  }"
  false

let%test "test_typecheck_mapping_4" = test_typecheck 
  "contract C {
      // mappings cannot be local variables
      function f() public { mapping (uint => uint) m; m[0] = 1; }
  }"
  false

let%test "test_typecheck_mapping_5" = try 
  "contract C {
      // mappings cannot be local variables
      function f(mapping (uint => uint) m) public { m[0] = 1; }
  }"
  |> parse_contract |> typecheck_contract |> fun _ ->  false
  with _ -> true

let%test "test_typecheck_mapping_6" = test_typecheck 
  "contract C {
      mapping (uint => uint) m;
      function f() public { m = 1; }
  }"
  false

let%test "test_typecheck_mapping_7" = test_typecheck 
  "contract C {
      mapping (uint => uint) m;
      function f() public { m[0] = m; }
  }"
  false

let%test "test_typecheck_mapping_8" = test_typecheck 
  "contract C {
      mapping (uint => uint) m;
      function f(int k) public { m[k] = 1; }
  }"
  false

let%test "test_typecheck_mapping_9" = test_typecheck 
  "contract C {
      mapping (uint => uint) m;
      function f(int k) public { m[uint(k)] = 1; }
  }"
  true

let%test "test_typecheck_mapping_10" = test_typecheck 
  "contract C {
      mapping (uint => uint) m;
      int x;
      function f(uint k) public { x = m[k]; }
  }"
  false

let%test "test_typecheck_mapping_11" = test_typecheck 
  "contract C {
      mapping (uint => uint) m;
      int x;
      function f(uint k) public { x = int(m[k]); }
  }"
  true


let%test "test_typecheck_immutable_1" = test_typecheck 
  "contract C {
      int immutable y;
      function f(int k) public { y = k; }
  }"
  false

let%test "test_typecheck_immutable_2" = test_typecheck 
  "contract C {
      int immutable y;
      function f(int k) public { if (k>0) y = k; else k = y; }
  }"
  false

let%test "test_typecheck_immutable_3" = test_typecheck 
  "contract C {
      uint immutable y;
      constructor() { y = 7; }
  }"
  true


let%test "test_typecheck_payable_1" = test_typecheck 
  "contract C { address payable a; function f() public payable { a.transfer(address(this).balance); } }"
  true

let%test "test_typecheck_payable_2" = test_typecheck 
  "contract C { function f(address payable a) public payable { a.transfer(address(this).balance); } }"
  true

let%test "test_typecheck_payable_3" = test_typecheck 
  "contract C { address a; function f() public payable { a.transfer(address(this).balance); } }"
  false

let%test "test_typecheck_payable_4" = test_typecheck 
  "contract C { function f(address a) public payable { a.transfer(address(this).balance); } }"
  false

let%test "test_typecheck_payable_5" = test_typecheck 
  "contract C { uint x; address payable a; function f() public payable { a.transfer(x); } }"
  true

let%test "test_typecheck_payable_6" = test_typecheck 
  "contract C { int x; address payable a; function f() public payable { a.transfer(x); } }"
  false

let%test "test_typecheck_addresscast_1" = test_typecheck 
  "contract C {
      uint immutable y;
      function f() public { address(\"0\").transfer(y); }
  }"
  false
