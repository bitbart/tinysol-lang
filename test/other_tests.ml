open Semantics
open Typechecker

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

let%test "test_mutability_1" = test_exec_tx
  "contract C {
      uint x;
      function f() public { x = 1; }
  }"
  ["0xA:0xC.f()"] 
  [("x==1");]

let%test "test_mutability_2" = test_exec_tx
  "contract C {
      uint x;
      function f() public view { x = 1; }
  }"
  ["0xA:0xC.f()"] 
  [("x==0");] (* f cannot be declared as view because it (potentially) modifies the state *)

let%test "test_mutability_3" = test_exec_tx
  "contract C {
      uint x;
      function f() public pure returns(uint) { return (x+1); }
      function g() public { x = this.f(); }
  }"
  ["0xA:0xC.g()"] 
  [("x==0");] (* f cannot be declared as pure because it reads the state *)

let%test "test_mutability_4" = test_exec_tx
  "contract C {
      uint x;
      function f() public view returns(uint) { return (x+1); }
      function g() public { x = this.f(); }
  }"
  ["0xA:0xC.g()"] 
  [("x==1");]

let%test "test_mutability_5" = test_exec_tx
  "contract C {
    function f() public { }
  }"
  ["0xA:0xC.f{value:1}()"] 
  [("this.balance==0");] (* wei can be sent only to payable functions *) 

let%test "test_mutability_6" = test_exec_tx
  "contract C {
    uint x;
    function f() public { require(msg.value==0); x=1; }
  }"
  ["0xA:0xC.f{value:0}()"] 
  [("x==0");] (* msg.value can only be used in payable functions *)

let%test "test_receive_1" = test_exec_fun
  "contract C { 
      uint x; 
      receive() external payable { x += 1; }
  }"
  "contract D { 
      constructor() payable { } 
      function f(address a) public { payable(a).transfer(1); }
  }"
  ["0xA:0xD.f(\"0xC\")"] 
  [("0xC","this.balance==1 && x==1"); ("0xD","this.balance==99")]

let%test "test_receive_2" = test_exec_fun
  "contract C { 
      D d;
      constructor() { d = \"0xD\"; }
      receive() external payable { d.g(); }
  }"
  "contract D { 
      uint x;
      constructor() payable { } 
      function f(address a) public { payable(a).transfer(1); }
      function g() public { x += 1; }
  }"
  ["0xA:0xD.f(\"0xC\")"] 
  [("0xC","this.balance==1"); ("0xD","this.balance==99 && x==0")]
  (* transfer does not carry enough gas to enable the call to d.g() *)
  (* Here the test passes, but just because the semantics of Send
     does not properly push frames on the call stack *)

let%test "test_typecheck_return_1" = test_typecheck
  "contract C {
    int x;
    function f() public view returns (uint) { return(x>0); }
  }"
  false

let%test "test_typecheck_return_2" = test_typecheck
  "contract C {
    int x;
    function f(bool b) public view returns (uint) { return(!b); }
  }"
  false

let%test "test_typecheck_proccall_1" = test_typecheck
  "contract C {
    int x;
    function f(int y) public { x = y; }
    function g() public { this.f(); }
  }"
  false

let%test "test_typecheck_funcall_1" = test_typecheck
  "contract C {
    uint x;
    function f() public pure returns(int) { return(1); }
    function g() public { bool b; b = this.f(); }
  }"
  false

let%test "test_typecheck_visibility_1" = test_typecheck
  "contract C {
    uint external x;
    function g() public { x += 1; }
  }"
  false

let%test "test_typecheck_visibility_2" = try test_typecheck
  "contract C {
    uint external x;
    function f() public { x += 1; }
  }" false (* state variables cannot have external visibility *)
  with _ -> true (* it is also ok if the contract is not parsable *)

let%test "test_typecheck_receive_1" = test_typecheck
  "contract C {
    receive() external payable { }
  }"
  true

let%test "test_typecheck_receive_2" = test_typecheck
  "contract C {
    receive() public payable { }
  }"
  false

let%test "test_typecheck_receive_3" = test_typecheck
  "contract C {
    receive() external { }
  }"
  false

let%test "test_typecheck_receive_4" = test_typecheck
  "contract C {
    receive(int x) external payable { }
  }"
  false

(* internal calls are not implemented yet *)
(* 
let%test "test_typecheck_visibility_2" = test_typecheck
  "contract C {
    uint x;
    function f() external { x += 1; }
    function g() public { f(); }
  }"
  false (* f is declared as external, so it cannot be invoked through an internal call *)
*)

let%test "test_typecheck_constant_1" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() { } 
    function f(int n) external { }
  }"
  true

let%test "test_typecheck_constant_2" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() { } 
    function f(int n) external { N=2; }
  }"
  false

let%test "test_typecheck_constant_3" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() { N=2; } 
    function f(int n) external { }
  }"
  false

let%test "test_typecheck_constant_4" = test_typecheck
  "contract C {
    int constant N;
    constructor() { } 
    function f(int n) external { }
  }"
  false