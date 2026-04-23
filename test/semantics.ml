open TinysolLib.Ast
open TinysolLib.Sysstate       
open TinysolLib.Utils
open TinysolLib.Main

(********************************************************************************
 test_exec_cmd : (command, n_steps, variable, expected value after n_steps)
 ********************************************************************************)

let test_exec_cmd (c,n_steps,var,exp_val) =
  c
  |> parse_cmd
  |> blockify_cmd (* TODO: enumify? *)
  |> fun c -> exec_cmd n_steps c (push_callstack {callee="0xC"; locals=[];} init_sysstate)
  |> fun t -> match t with
  | St st -> Option.get (lookup_var var st) = exp_val
  | CmdSt(_,st) -> Option.get (lookup_var var st) = exp_val
  | Reverted _ -> false
  | Returned _ -> false

let%test "test_exec_cmd_1" = test_exec_cmd
  ("{ int x; x=51; skip; }", 2, "x", Int 51)  

let%test "test_exec_cmd_2" = test_exec_cmd
  ("{ int x; x=51; x=x+1; skip; }", 5, "x", Int 52)  

let%test "test_exec_cmd_3" = test_exec_cmd
  ("{ int x; x=51; { int x; x=42; } x=x+1; skip; }", 7, "x", Int 52)  

let%test "test_exec_cmd_4" = test_exec_cmd
  ("{ int x; x=51; { int x; x=42; skip; skip; skip; } x=x+1; skip; }", 5, "x", Int 42)  

let%test "test_exec_cmd_5" = test_exec_cmd
  ("{ int x; x=51; { int x; x=x+1; skip; skip; skip; } x=x+1; skip; }", 6, "x", Int 1)  

let%test "test_exec_cmd_6" = test_exec_cmd
  ("{ int x; x=51; { int x; x=1; } { int x; x=x+3; } x=x+5; skip; }", 11, "x", Int 56)  

let%test "test_exec_cmd_7" = test_exec_cmd
  ("{ int x; x=51; { int y; y=x+1; skip; } { x = 0; } x=x+5; skip; }", 12, "x", Int 5)  

let%test "test_exec_cmd_8" = test_exec_cmd
  ("{ int x; x=51; { int y; y=x+1; skip; } { int x; x = 0; } x=x+5; skip; }", 12, "x", Int 56)  

let%test "test_exec_cmd_9" = test_exec_cmd
  ("{ bool b; b = 2==2; skip; }", 3, "b", Bool true)  

let%test "test_exec_cmd_10" = test_exec_cmd
  ("{ int x; if (x==0) x=1; else x=2; skip; }", 5, "x", Int 1)  

let%test "test_exec_cmd_11" = test_exec_cmd
  ("{ int x; if (x>0) x=1; else x=2; skip; }", 5, "x", Int 2)  

let%test "test_exec_cmd_12" = test_exec_cmd
  ("{ int x; bool b; if (b) x=2; else b=true; skip; }", 2, "x", Int 0)  

let%test "test_exec_cmd_13" = test_exec_cmd
  ("{ int x; uint y; y=5; x = (y>5)?2:3; skip; }", 6, "x", Int 3)  

let%test "test_exec_cmd_14" = test_exec_cmd
  ("{ int x; uint y; y=5; x = (y<=5)?2:3; skip; }", 6, "x", Int 2)  

let%test "test_exec_cmd_15" = test_exec_cmd
  ("{ int x; uint y; y=5; x = (y<=(x==0)?5:4)?2:3; skip; }", 9, "x", Int 2)  

let%test "test_exec_cmd_16" = test_exec_cmd
  ("{ int x; uint y; y=5; x = (y<=(x!=0)?5:4)?2:3; skip; }", 9, "x", Int 3)  

let%test "test_exec_cmd_17" = test_exec_cmd
  ("{ int x; uint y1; int y0; y1=5; y0=8; x = (true)?int(y1):y0; skip; }", 7, "x", Int 5)  

let%test "test_exec_cmd_18" = test_exec_cmd
  ("{ uint x; uint y; y = 6; x = (y>5)?uint(y):1 + 3; skip; }", 8, "x", Uint 6) 

let%test "test_exec_cmd_19" = test_exec_cmd
  ("{ uint x; x=1; { int x; x=x+1; { int x; x=x+2; } x=x+3; } x=x+4; skip; }", 16, "x", Uint 5) 

(********************************************************************************
 test_exec_tx : 
 - src: the contract that will be deployed for testing,
 - txl: a list of transactions that will be executed
 - els: a list of assertions expected to be true
 ********************************************************************************)

let test_exec_tx (src: string) (txl: string list) (els : string list) =
  let txl = List.map parse_transaction txl in
  init_sysstate
  |> faucet "0xA" 100
  |> faucet "0xB" 100
  |> deploy_contract { txsender="0xA"; txto="0xC"; txfun="constructor"; txargs=[]; txvalue=0; } src 
  |> exec_tx_list 1000 txl 
  |> fun st -> List.map (fun x -> x |> parse_expr |> eval_expr 
      { st with callstack = [{ callee = "0xC"; locals = []}] } ) els 
  |> List.for_all (fun v -> v = Bool true)

let c1 = "contract C {
    int x;
    bool b;
  
    function g() public { 
        if (b) x = x+1;
        else b=true;
    }
}"

let%test "test_if_else_1" = test_exec_tx
  c1
  ["0xA:0xC.g()"] 
  ["x==0"; "b"]

let%test "test_if_else_2" = test_exec_tx
  c1
  ["0xA:0xC.g()"; "0xA:0xC.g()"] 
  ["x==1"; "b"]

let%test "test_if_else_3" = test_exec_tx
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

let%test "test_require_1" = test_exec_tx
  c2
  ["0xA:0xC.f()"] 
  ["x==1"; "owner==\"0xA\""]  

let%test "test_require_2" = test_exec_tx
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

let%test "test_require_3" = test_exec_tx
  c3
  ["0xA:0xC.f()"] 
  ["x==0"; "this.balance==0"]

let%test "test_require_4" = test_exec_tx
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

let%test "test_require_5" = test_exec_tx
  c4
  ["0xA:0xC.f()"] 
  ["x==0"; "this.balance==0"]

let%test "test_require_6" = test_exec_tx
  c4
  ["0xA:0xC.f{value : 3}()"] 
  ["x==1"; "this.balance==3"]


let%test "test_arith_1" = test_exec_tx
  "contract C {
      int x;
      int y;
      function f(int x) public { int x; y=x; x=3; }
      function g(int y) public { x=y; y=-1; }
  }"
  ["0xA:0xC.g(1)"; "0xA:0xC.f(2)"] 
  ["x==1"; "y==0"]

let%test "test_arith_2" = test_exec_tx
  "contract C {
      bool b;
      function f(int x, int y) public { b = x<y; }
  }"
  ["0xA:0xC.f(3,4)"] 
  ["b==true"]

let%test "test_arith_3" = test_exec_tx
  "contract C {
      bool b;
      function f(int x, int y) public { b = x<y; }
  }"
  ["0xA:0xC.f(3,2)"] 
  ["b==false"]

let%test "test_arith_4" = test_exec_tx
  "contract C {
      int y;
      function f(int x) public { y = 2*(x+1)-1 + (-2); }
  }"
  ["0xA:0xC.f(3)"] 
  ["y==5"]

let%test "test_arith_5" = test_exec_tx
  "contract C {
      uint x;
      function f(int y) public { x=7; x = uint(y)-1; }
  }"
  ["0xA:0xC.f(3)"; "0xA:0xC.f(0)"] 
  ["x==2"]

let%test "test_arith_6" = test_exec_tx
  "contract C {
      uint x;
      function f(uint y,uint z) public { x = (y-1)+z; }
  }"
  ["0xA:0xC.f(1,2)"] 
  ["x==2"]

let%test "test_arith_7" = test_exec_tx
  "contract C {
      uint x;
      function f(uint y,uint z) public { x = (y-1)+z; }
  }"
  ["0xA:0xC.f(0,3)"] 
  ["x==0"]

let%test "test_mapping_1" = test_exec_tx
  "contract C {
      mapping (uint => uint) m;
      uint x;
      function g(int k) public { x= (m[k]==0)?1:2; }
  }"
  ["0xA:0xC.g(0)"] 
  ["x==1"]

let%test "test_mapping_2" = test_exec_tx
  "contract C {
      mapping (address => uint) m;
      uint x;
      function f(address a, uint n) public { m[a] = n; }
  }"
  ["0xA:0xC.f(\"0xA\",1)"; "0xA:0xC.f(\"0xB\",2)"; "0xA:0xC.f(\"0xA\",3)"] 
  ["m[\"0xA\"]==3"; "m[\"0xB\"]==2"]

let%test "test_mapping_3" = test_exec_tx
  "contract C {
      mapping (int => int) m;
      int x;
      function f(int k, int v) public { m[k] = v; }
      function g(int k) public { x = m[k]; }
  }"
  ["0xA:0xC.f(0,1)"; "0xA:0xC.g(0)"] 
  ["x==1"]

let%test "test_mapping_4" = test_exec_tx
  "contract C {
      mapping (uint => uint) m1;
      mapping (uint => uint) m2;
      function f(uint k, uint v) public { m1[k] = v; }
      function g(uint k, uint v) public { m2[m1[k]] = m1[m2[0]+v]; }
  }"
  ["0xA:0xC.f(1,2)"; "0xA:0xC.g(1,1)"] 
  ["m2[2]==2"]

let%test "test_mapping_5" = test_exec_tx
  "contract C {
      mapping (uint => uint) m;
      function f(uint k) public { m[m[k]] = m[k]+1; }
  }"
  ["0xA:0xC.f(0)"; "0xA:0xC.f(0)"] 
  ["m[0]==1"; "m[1]==2"]

let%test "test_enum_1" = test_exec_tx
  "contract C { enum State {IDLE,REQ} State s; function f() public { s = State.REQ; } }"
  ["0xA:0xC.f()"] 
  ["s==State.REQ"]

let%test "test_enum_2" = test_exec_tx
  "contract C { enum State {IDLE,REQ} State s; function f() public { s = State(1); } }"
  ["0xA:0xC.f()"] 
  ["s==State.REQ"]

let%test "test_enum_3" = test_exec_tx
  "contract C { enum State {IDLE,REQ} State s; function f() public { s = State(2); } }"
  ["0xA:0xC.f()"] 
  ["s==State.IDLE"]

let%test "test_enum_4" = test_exec_tx
  "contract C { 
    enum State {Q0,Q1} 
    State s;
    function f(int y) public { if (s==State(y)) s=State.Q1; else s=State.Q0; } 
  }"
  ["0xA:0xC.f(0)"] 
  ["s==State.Q1"]

let%test "test_enum_5" = test_exec_tx
  "contract C { 
    enum State {Q0,Q1} 
    State s;
    function f(int y) public { int q; q = (y==0)?1:0; if (s==State(y)) s=State(q); } 
  }"
  ["0xA:0xC.f(0)"; "0xA:0xC.f(1)"; "0xA:0xC.f(2)"] 
  ["s==State.Q0"]

let%test "test_enum_6" = test_exec_tx
  "contract C { 
    enum State {Q0,Q1} 
    State s;
    function f(int y) public { int q; q = (y==0)?1:0; if (s==State(y)) s=State(q); } 
  }"
  ["0xA:0xC.f(0)"; "0xA:0xC.f(1)"; "0xA:0xC.f(2)"; "0xA:0xC.f(0)"] 
  ["s==State.Q1"]

let%test "test_block_1" = test_exec_tx
  "contract C {
    uint y;
    function f() public { 
      uint x; x=1; { int x; x=x+1; { int x; x=x+2; } x=x+3; } x=x+4; y=x;
    }
  }"
  ["0xA:0xC.f()"] 
  ["y==5"]

let%test "test_block_2" = test_exec_tx
  "contract C {
    uint x;
    uint y;
    uint z;
    constructor() { x=100; y=200; }
    function f(uint y) public { 
      uint x; x=1; z=x; { int x; x=x+1; { int x; x=x+2; } x=x+3; z=z+uint(x); } x=x+4; y=x;
    }
  }"
  ["0xA:0xC.f(1)"] 
  ["x==100 && y==200 && z==5"]

let test_exec_constructor (src: string) (value: int) (args: exprval list) (els : string list) =
  init_sysstate
  |> faucet "0xA" 100
  |> deploy_contract { txsender="0xA"; txto="0xC"; txfun="constructor"; txargs=args; txvalue=value; } src 
  |> fun st -> List.map (fun x -> x |> parse_expr |> eval_expr 
      { st with callstack = [{ callee = "0xC"; locals = []}] } ) els 
  |> List.for_all (fun v -> v = Bool true)

let%test "test_constructor_1" = test_exec_constructor
  "contract C { 
    uint x; 
    constructor() { x+=1; } 
  }"
  0 [] 
  ["x==1"]

let%test "test_constructor_2" = test_exec_constructor
  "contract C { 
    uint x; 
    constructor() { require(msg.sender == \"0xA\"); x = 1; } 
  }"
  0 [] 
  ["x==1"]

let%test "test_constructor_3" = test_exec_constructor
  "contract C { 
    uint x; 
    constructor() payable { if (msg.sender != \"0xA\") x = 1; else x = 3; } 
  }"
  1 [] 
  ["x==3"]

let%test "test_constructor_4" = test_exec_constructor
  "contract C { 
    uint x; 
    constructor() payable { require(msg.value >= 0); x = 1; } 
  }"
  1 [] 
  ["x==1"]

let%test "test_constructor_5" = test_exec_constructor
  "contract C { 
    uint x; 
    constructor() payable { x += msg.value; } 
  }"
  3 [] 
  ["x==3"]

let%test "test_constructor_6" = test_exec_constructor
  "contract C { 
    uint x; 
    constructor(uint _x) payable { x = _x; } 
  }"
  0 [Uint 3] 
  ["x==3"]

let%test "test_constructor_7" = test_exec_constructor
  "contract C { 
    uint x; 
    constructor(uint y, int z) payable { x = y + uint(z); } 
  }"
  0 [Uint 2; Int 1] 
  ["x==3"]


(********************************************************************************
 test_exec_fun : 
 - src1, src2: the contracts that will be deployed for testing,
 - txl: a list of transactions that will be executed
 - els: a list of (contract,assertions) expected to be true
 ********************************************************************************)

let test_exec_fun (src1: string) (src2: string) (txl : string list) (els : (addr * string) list) =
  let txl = List.map parse_transaction txl in
  init_sysstate
  |> faucet "0xA" 200
  |> faucet "0xB" 100
  |> deploy_contract { txsender="0xA"; txto="0xC"; txfun="constructor"; txargs=[]; txvalue=0; } src1 
  |> deploy_contract { txsender="0xA"; txto="0xD"; txfun="constructor"; txargs=[]; txvalue=100; } src2 
  |> exec_tx_list 1000 txl 
  |> fun st -> List.map (fun (a,x) -> x |> parse_expr |> eval_expr 
    { st with callstack = [ { callee = a; locals = [] } ] }) els 
  |> List.for_all (fun v -> v = Bool true)

let%test "test_proc_1" = test_exec_fun
  "contract C { uint x; function f() public { x+=1;} }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } function g() public { c.f(); } }"
  ["0xA:0xD.g()"] 
  [("0xC","x==1"); ("0xD","x==0")]

let%test "test_proc_2" = test_exec_fun
  "contract C { uint x; function f() public { x+=1;} }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } function g() public { c.f(); } }"
  ["0xA:0xD.g()"; "0xA:0xD.g()"] 
  [("0xC","x==2"); ("0xD","x==0")]

let%test "test_proc_3" = test_exec_fun
  "contract C { uint x; function f() public { x+=1; require(x==0); } }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } function g() public { x=2; c.f(); } }"
  ["0xA:0xD.g()"] 
  [("0xC","x==0"); ("0xD","x==0")]

let%test "test_proc_4" = test_exec_fun
  "contract C { uint x; function f(uint n) public { x+=n; } }"
  "contract D { C c; constructor() payable { c = \"0xC\"; } function g() public { c.f(1); } }"
  ["0xA:0xD.g()"] 
  [("0xC","x==1")]

let%test "test_proc_5" = test_exec_fun
  "contract C { uint x; function f(uint n) public { x+=n; } }"
  "contract D { C c; constructor() payable { c = \"0xC\"; } function g() public { c.f(1); } }"
  ["0xA:0xD.g()"; "0xA:0xD.g()"] 
  [("0xC","x==2")]

let%test "test_proc_6" = test_exec_fun
  "contract C { function f() public payable { require(msg.value > 0); } }"
  "contract D { C c; constructor() payable { c = \"0xC\"; } function g() public { c.f{value:1}(); } }"
  ["0xA:0xD.g()"; "0xA:0xD.g()"] 
  [("0xC","this.balance==2"); ("0xD","this.balance==98")]

let%test "test_proc_7" = test_exec_fun
  "contract C { uint x; function f(uint n1, uint n2) public { x+=n1+n2; } }"
  "contract D { C c; constructor() payable { c = \"0xC\"; } function g() public { c.f(1+2,3+4); } }"
  ["0xA:0xD.g()"] 
  [("0xC","x==10")]

let%test "test_proc_8" = test_exec_fun
  "contract C {  
      function f() public payable { }
      function g(uint amt) public { msg.sender.transfer(amt); } 
  }"
  "contract D { 
      C c; uint x; 
      constructor() payable { c = \"0xC\"; } 
      function dp(uint amt) public { c.f{value:amt}(); }
      function wd(uint amt) public { c.g(amt); } 
  }"
  ["0xA:0xD.dp(10)"] 
  [("0xC","this.balance==10"); ("0xD","this.balance==90")]

let%test "test_proc_9" = test_exec_fun
  "contract C {  
      function f() public payable { }
      function g(uint amt) public { msg.sender.transfer(amt); } 
  }"
  "contract D { 
      C c; uint x; 
      constructor() payable { c = \"0xC\"; } 
      function dp(uint amt) public { c.f{value:amt}(); }
      function wd(uint amt) public { c.g(amt); } 
  }"
  ["0xA:0xD.dp(10)"; "0xA:0xD.wd(5)"] 
  [("0xC","this.balance==5"); ("0xD","this.balance==95")]

let%test "test_fun_1" = test_exec_fun
  "contract C { function f() public returns(int) { return(1); } }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } function g() public { x = c.f(); } }"
  ["0xA:0xD.g()"] 
  [("0xD","x==1")]

let%test "test_fun_2" = test_exec_fun
  "contract C { function f() public returns(int) { return(1+2+3+4);} }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } function g() public { x = c.f(); } }"
  ["0xA:0xD.g()"] 
  [("0xD","x==10")]

let%test "test_fun_3" = test_exec_fun
  "contract C { uint y; function f() public returns(int) { y=5; return(1);} }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } function g() public { x = c.f(); } }"
  ["0xA:0xD.g()"] 
  [("0xC","y==5"); ("0xD","x==1")]

let%test "test_fun_3" = test_exec_fun
  "contract C { uint y; function f() public returns(int) { y=123; return(y+1);} }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } function g() public { x = c.f(); } }"
  ["0xA:0xD.g()"] 
  [("0xC","y==123"); ("0xD","x==124")]

let%test "test_fun_4" = test_exec_fun
  "contract C { uint y; function f() public payable returns(int) { y=8; return(y+1);} }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } function g() public { x = c.f{value:1}(); } }"
  ["0xA:0xD.g()"] 
  [("0xC","y==8 && this.balance==1"); ("0xD","x==9 && this.balance==99")]

let%test "test_fun_5" = test_exec_fun
  "contract C { uint y; function f(int z) public payable returns(int) { y=uint(z)+1; return(z+1+int(msg.value));} }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } function g() public { x = uint(c.f{value:2}(1)); } }"
  ["0xA:0xD.g()"] 
  [("0xC","y==2 && this.balance==2"); ("0xD","x==4 && this.balance==98")]

let%test "test_fun_6" = test_exec_fun
  "contract C { D d; uint y; constructor() payable { d = \"0xD\"; } 
      function f() public returns(int) { y = d.h(); return(y+1); } }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } 
      function g() public { x = c.f()+1; }
      function h() public returns(int) { return (1+1); } }"
  ["0xA:0xD.g()"] 
  [("0xC","y==2"); ("0xD","x==4")]

let%test "test_fun_7" = test_exec_fun
  "contract C { uint y; constructor() payable { } 
      function f(uint x1, uint x2, uint x3) public returns(uint) { y = x1+x2+x3; return(2*y); } 
  }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } 
      function g() public { x = c.f(1+1,1+1+1,1+1+1+1); }
  }"
  ["0xA:0xD.g()"] 
  [("0xC","y==9"); ("0xD","x==18")]

let%test "test_fun_8" = test_exec_fun
  "contract C { int y; constructor() payable { y=1; } 
      function f(int y) public returns(int) { return(2*y); } 
  }"
  "contract D { C c; int x; constructor() payable { c = \"0xC\"; } 
      function g() public { x = c.f(2); }
  }"
  ["0xA:0xD.g()"] 
  [("0xC","y==1"); ("0xD","x==4")]

let%test "test_fun_9" = test_exec_fun
  "contract C { int y; constructor() payable { y=1; } 
      function f(int z) public returns(int) { if (z>0) return(z+y); else return(-z+y); } 
  }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } 
      function g() public { x = uint(c.f(-2)); }
  }"
  ["0xA:0xD.g()"] 
  [("0xC","y==1"); ("0xD","x==3")]


let%test "test_fun_10" = test_exec_fun
  "contract C { constructor() payable { } 
      function f() public returns(int) { { return(2); } } 
  }"
  "contract D { C c; uint x; constructor() payable { c = \"0xC\"; } 
      function g() public { x = c.f(); }
  }"
  ["0xA:0xD.g()"] 
  [("0xD","x==2")]

let%test "test_fun_11" = test_exec_fun
  "contract C { int y; constructor() payable { y=5; } 
      function f() public returns(int) { if (msg.sender==\"0xD\") { int z; z=y; y=2; return(z); } else return(-1); } 
  }"
  "contract D { C c; int x; constructor() payable { c = \"0xC\"; } 
      function g() public { x = c.f(); }
  }"
  ["0xA:0xD.g()"] 
  [("0xC","y==2"); ("0xD","x==5")]

let%test "test_fun_12" = test_exec_fun
  "contract C { uint x; constructor() payable { x=5; } 
      function f() public { x=7; } 
  }"
  "contract D { C c; uint y; constructor() payable { c = \"0xC\"; } 
      function g() public { { uint x; x=1; c.f(); y=x; } }
  }"
  ["0xA:0xD.g()"] 
  [("0xC","x==7"); ("0xD","y==1")]

let%test "test_fun_13" = test_exec_fun
  "contract C { D d; uint x; constructor() payable { d = \"0xD\"; x=5; } 
      function f() public { x = d.h(); } 
  }"
  "contract D { C c; int y; constructor() payable { c = \"0xC\"; } 
      function g() public { { int x; x=1; c.f(); y=x; } }
      function h() public returns(uint) { return 7; }
  }"
  ["0xA:0xD.g()"] 
  [("0xC","x==7"); ("0xD","y==1")]

let%test "test_fun_14" = test_exec_fun
  "contract C { D d; uint x; constructor() payable { d = \"0xD\"; x=5; } 
      function f() public { x=d.h(); } 
  }"
  "contract D { C c; int y; constructor() payable { c = \"0xC\"; } 
      function g() public { { int x; x=1; c.f(); y=x; } }
      function h() public returns(uint) { return this.k()+1; }
      function k() public returns(uint) { return 6; }
  }"
  ["0xA:0xD.g()"] 
  [("0xC","x==7"); ("0xD","y==1")]

let%test "test_fun_15" = test_exec_fun
  "contract C { 
      function fact(uint n) public returns(uint) { return (n==0) ? 1 : (n * this.fact(n-1)); } 
  }"
  "contract D { C c; uint y; constructor() payable { c = \"0xC\"; } 
      function g(int x) public { y = c.fact(x); }
  }"
  ["0xA:0xD.g(4)"] 
  [("0xD","y==24")]

let%test "test_fun_16" = test_exec_fun
  "contract C { D d; constructor() payable { d = \"0xD\"; } 
      function f(int x) public returns(uint) { return 1 + (x==0) ? 0 : d.g(x-1); } 
  }"
  "contract D { C c; uint y; constructor() payable { c = \"0xC\"; } 
      function g(int x) public returns(uint) { if (x==0) return 0; else return 1 + c.f(x-1); }
      function h(int x) public { y = this.g(x); }
  }"
  ["0xA:0xD.h(3)"] 
  [("0xD","y==4")]

let%test "test_fun_17" = test_exec_fun
  "contract C { D d; uint y; constructor() payable { d = \"0xD\"; } 
      function f(int x) public { y=3; d.h(x); } 
  }"
  "contract D { C c; uint y; constructor() payable { c = \"0xC\"; } 
      function g(int x) public { c.f(x); }
      function h(int x) public { require(x==0 || msg.sender != c); y=4; }
  }"
  ["0xA:0xD.g(0)"] 
  [("0xC","y==3"); ("0xD","y==4")]

let%test "test_fun_18" = test_exec_fun
  "contract C { D d; uint y; constructor() payable { d = \"0xD\"; } 
      function f(int x) public { y=3; d.h(x); } 
  }"
  "contract D { C c; uint y; constructor() payable { c = \"0xC\"; } 
      function g(int x) public { c.f(x); }
      function h(int x) public { require(x==0 || msg.sender != c); y=4; }
  }"
  ["0xA:0xD.g(1)"] 
  [("0xC","y==0"); ("0xD","y==0")]

let%test "test_fun_19" = test_exec_fun
  "contract C { D d; uint y; constructor() payable { d = \"0xD\"; } 
      function f() public { y=y+1; return (y-1); }      
  }"
  "contract D { C c; uint y; constructor() payable { c = \"0xC\"; } 
      function g(int x) public { require(c.f() != 0); y=4; }
  }"
  ["0xA:0xD.g(1)"] 
  [("0xC","y==0"); ("0xD","y==0")]

let%test "test_fun_20" = test_exec_fun
  "contract C { D d; constructor() payable { d = \"0xD\"; } 
      function f() public payable { if (address(d).balance>0) d.g(); }
  }"
  "contract D { C c; uint y; constructor() payable { c = \"0xC\"; } 
      function g() public { c.f{value : 1}(); }
  }"
  ["0xA:0xD.g()"] 
  [("0xC","this.balance==100"); ("0xD","this.balance==0")]


(********************************************************************************
 * Shortcut semantics
 ********************************************************************************)

let%test "test_shortcut_1" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (x==1 || this.g()==1) x+=1; else x=5; }
      function g() public returns(uint) { require(x==0); return 1; } 
  }"
  ["0xA:0xC.f()"] 
  [("x==1");]

let%test "test_shortcut_2" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (x==1 || this.g()==1) x+=1; else x=5; }
      function g() public returns(uint) { require(x==0); return 1; } 
  }"
  ["0xA:0xC.f()"; "0xA:0xC.f()"] 
  [("x==2");]

let%test "test_shortcut_3" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (x==0 && this.g()==1) x=1; else x=5; }
      function g() public returns(uint) { require(x==0); return 1; }
  }"
  ["0xA:0xC.f()"] 
  [("x==1");]

let%test "test_shortcut_4" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (x==0 && this.g()==1) x=1; else x=5; }
      function g() public returns(uint) { require(x==0); return 1; }
  }"
  ["0xA:0xC.f()"; "0xA:0xC.f()"] 
  [("x==5");]

let%test "test_shortcut_5" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (x==0 || this.g()==1) x+=1; else x+=2; }
      function g() public returns(uint) { require(false); x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==1");]

let%test "test_shortcut_6" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (false || this.g()==1) x+=1; else x+=2; }
      function g() public returns(uint) {x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==11");]

let%test "test_shortcut_7" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (false || this.g()==0) x+=1; else x+=2; }
      function g() public returns(uint) {x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==12");]

  let%test "test_shortcut_8" = test_exec_tx
  "contract C {
      uint x;
      uint y;
      function f() public { 
          x = 1;
          if (x==1 || this.g()==5) { x = x + 1; }
      }
      function g() public returns(uint) { 
          y = y + 1;
          return 5; 
      }
  }"
  ["0xA:0xC.f()"]
  ["x==2"; "y==0"]

  let%test "test_shortcut_9" = test_exec_tx
  "contract C {
      uint x;
      uint y;
      function f() public { 
          x = 1;
          if (x==2 || this.g()==5) { x = x + 1; }
      }
      function g() public returns(uint) { 
          y = y + 1;
          return 5; 
      }
  }"
  ["0xA:0xC.f()"]
  ["x==2"; "y==1"]

let%test "test_shortcut_10" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (false && this.g()==0) x+=1; else x+=2; }
      function g() public returns(uint) {x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==2");]

let%test "test_shortcut_11" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (true && this.g()==0) x+=1; else x+=2; }
      function g() public returns(uint) {x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==12");]

let%test "test_shortcut_12" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (true && this.g()==1) x+=1; else x+=2; }
      function g() public returns(uint) {x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==11");]

let%test "test_shortcut_13" = test_exec_tx
  "contract C {
      uint x;
      uint y;
      function f() public { 
        x = 1;
        if (x==3 && this.g()==5) { x = x + 1; }
      }
      function g() public returns(uint) { 
        y = y + 1;
        return 5; 
      }
  }"
  ["0xA:0xC.f()"]
  ["x==1"; "y==0"]

  let%test "test_shortcut_14" = test_exec_tx
  "contract C {
      uint x;
      uint y;
      function f() public { 
        x = 1;
        if (x==1 && this.g()==5) { x = x + 1; } //(x = 1+1)
      }
      function g() public returns(uint) { 
        y = y + 1;
        return 5; 
      }
  }"
  ["0xA:0xC.f()"]
  ["x==2"; "y==1"]

let%test "test_shortcut_15" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return true; }
      function add01000() public returns(bool) { x+=8; return true; }
      function add00100() public returns(bool) { x+=4; return true; }
      function f() public {x=0; if (this.add10000() || this.add01000() || this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==18")]

let%test "test_shortcut_16" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return false; }
      function add01000() public returns(bool) { x+=8; return true; }
      function add00100() public returns(bool) { x+=4; return true; }
      function f() public {x=0; if (this.add10000() || this.add01000() || this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==26")]

let%test "test_shortcut_17" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return false; }
      function add01000() public returns(bool) { x+=8; return false; }
      function add00100() public returns(bool) { x+=4; return true; }
      function f() public {x=0; if (this.add10000() || this.add01000() || this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==30")]

let%test "test_shortcut_18" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return false; }
      function add01000() public returns(bool) { x+=8; return false; }
      function add00100() public returns(bool) { x+=4; return false; }
      function f() public {x=0; if (this.add10000() || this.add01000() || this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==29")]

let%test "test_shortcut_19" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return false; }
      function add01000() public returns(bool) { x+=8; return false; }
      function add00100() public returns(bool) { x+=4; return false; }
      function f() public {x=0; if (this.add10000() && this.add01000() && this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==17")]

let%test "test_shortcut_20" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return true; }
      function add01000() public returns(bool) { x+=8; return false; }
      function add00100() public returns(bool) { x+=4; return false; }
      function f() public {x=0; if (this.add10000() && this.add01000() && this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==25")]

let%test "test_shortcut_21" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return true; }
      function add01000() public returns(bool) { x+=8; return true; }
      function add00100() public returns(bool) { x+=4; return false; }
      function f() public {x=0; if (this.add10000() && this.add01000() && this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==29")]

let%test "test_shortcut_22" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return true; }
      function add01000() public returns(bool) { x+=8; return true; }
      function add00100() public returns(bool) { x+=4; return true; }
      function f() public {x=0; if (this.add10000() && this.add01000() && this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==30")]