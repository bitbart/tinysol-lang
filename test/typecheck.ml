open TinysolLib.Utils
open TinysolLib.Static

let test_typecheck (src: string) (exp : bool)=
  let c = src |> parse_contract |> preprocess_contract in 
  match typecheck_contract c with
    | Ok() -> exp
    | _ -> not exp  

let%test "test_typecheck_0" = test_typecheck 
  "contract C0 { }"
  true

let%test "test_typecheck_1" = test_typecheck 
"contract C {
    int x;
    bool b;
  
    function g() public { 
        if (b) x = x+1;
        else b=true;
    }
}" 
true

let%test "test_typecheck_2" = test_typecheck
"contract C {
    int x;
    address owner;
  
    constructor() { owner = msg.sender; }

    function f() public { 
        require(msg.sender == owner);
        x = x+1;
    }
}"
true

let%test "test_typecheck_3" = test_typecheck
"contract C {
    int x;
  
    function f() public payable { 
        require(msg.value > 0);
        x = x+1;
    }
}"
true

let%test "test_typecheck_4" = test_typecheck 
"contract C {
    int x;
  
    function f() public payable { 
        x = x+1;
        require(x > 0 && msg.value > 0);
    }
}"
true

let%test "test_typecheck_base_1" = test_typecheck 
"contract C {
    int x;
    function f(bool b) public { x = b; }
}"
false

let%test "test_typecheck_base_2" = test_typecheck 
"contract C {
    int x;
    function f(bool x) public { x = true; }
}"
true

let%test "test_typecheck_base_3" = test_typecheck 
"contract C {
    int x;
    function f(bool x) public { x = 1; }
}"
false

let%test "test_typecheck_base_4" = test_typecheck 
"contract C {
    int x;
    function f() public { x = x+1; }
}"
true

let%test "test_typecheck_base_5" = test_typecheck 
"contract C {
    int x;
    function f() public { x = x-1; }
}"
true

let%test "test_typecheck_base_6" = test_typecheck 
"contract C {
    uint x;
    function f() public { x = x-1; }
}"
true

let%test "test_typecheck_base_7" = test_typecheck 
"contract C {
    address x;
    function f() public { x = x-1; }
}"
false

let%test "test_typecheck_base_8" = test_typecheck 
"contract C {
    int x;
    function f() public { x = -1; }
}"
true

let%test "test_typecheck_base_9" = test_typecheck 
"contract C {
    uint x;
    function f() public { x = -1; }
}"
false

let%test "test_typecheck_base_10" = test_typecheck 
"contract C {
    uint x;
    function f() public { x = 2-1; }
}"
true

let%test "test_typecheck_base_11" = test_typecheck 
"contract C {
    uint x;
    function f() public { x = 2+1; }
}"
true

let%test "test_typecheck_base_12" = test_typecheck 
"contract C {
    uint x;
    function f(int x) public { x = -1; }
}"
true

let%test "test_typecheck_base_14" = test_typecheck 
"contract C {
    int x;
    function f(uint x) public { x = -1; }
}"
false

let%test "test_typecheck_base_15" = test_typecheck 
"contract C {
    int x; bool b;
    function f(uint x) public { x = b; }
}"
false

let%test "test_typecheck_base_16" = test_typecheck 
"contract C {
    int x; bool b;
    function f(uint x) public { b = x; }
}"
false

let%test "test_typecheck_base_17" = test_typecheck 
"contract C {
    int x; address a;
    function f(uint x) public { x = a; }
}"
false

let%test "test_typecheck_base_18" = test_typecheck 
"contract C {
    int x; address a;
    function f(uint x) public { a = x; }
}"
false

let%test "test_typecheck_base_19" = test_typecheck 
"contract C {
    int x;
    function f(uint y) public { y = x; }
}"
false

let%test "test_typecheck_base_20" = test_typecheck 
"contract C {
    int x;
    // uint is not convertible to int
    function f(uint y) public { x = y; }
}"
false

let%test "test_typecheck_base_21" = test_typecheck 
"contract C {
    int x;
    // uint is not comparable to int
    function f(uint y) public { require (x < y); }
}"
false

let%test "test_typecheck_base_22" = test_typecheck 
"contract C {
    uint x;
    // uint is not comparable to int
    function f(int y) public { require (x < y); }
}"
false

let%test "test_typecheck_base_23" = test_typecheck 
"contract C {
    int x;
    function f(int y) public { require (x < y && y < -5); }
}"
true

let%test "test_typecheck_base_24" = test_typecheck 
"contract C {
    uint x;
    function f(uint y) public { require (x < y && y < 5); }
}"
true

let%test "test_typecheck_base_26" = test_typecheck 
"contract C {
    uint x;
    function f(uint y) public { require (x < y && y == x+5); }
}"
true

let%test "test_typecheck_base_27" = test_typecheck 
"contract C {
    int x;
    function f(int y) public { require (x < y && y != 5); }
}"
true

let%test "test_typecheck_base_28" = test_typecheck 
"contract C {
    uint x;
    function f(int y) public { if (true || false) x=1; else x=-1; }
}"
true (* this is not be type-checkable by solc *)

let%test "test_typecheck_base_29" = test_typecheck 
"contract C {
    uint x;
    function f(int y) public { x = 1+3; if (x>3) x=1; else x=-1; }
}"
false

let%test "test_typecheck_base_30" = test_typecheck 
"contract C {
    uint x;
    function f(int y) public { if (2<3 || false) x=1; else x=-1; }
}"
true (* this is not be type-checkable by solc *)

let%test "test_typecheck_cast_1" = test_typecheck 
"contract C {
    int x;
    function f(uint y) public { require uint(x) < y; x = int(y); }
}"
true

let%test "test_typecheck_cast_2" = test_typecheck 
"contract C {
    int x;
    function f(uint y) public { require int(y) < x; y = uint(x); }
}"
true

let%test "test_typecheck_cast_3" = test_typecheck 
"contract C {
    int x;
    function f(uint y) public { require int(y)+x < 7; y = uint(x) + 1; }
}"
true

let%test "test_typecheck_cast_4" = test_typecheck 
  "contract C {
      uint x;
      function f(int y) public { x=7; x = uint(y)-1; }
  }"
  true

let%test "test_typecheck_block_1" = test_typecheck 
"contract C {
    uint x;
    function f(int x) public { x = -1; { bool x; x = true; } x = x+1; }
}"
true

let%test "test_typecheck_block_2" = test_typecheck 
"contract C {
    int x;
    function f() public { { int y; y=x+1; } }
}"
true

let%test "test_typecheck_decl_1" = test_typecheck 
"contract C {
    int x;
    int x;
    function f(int y) public { require x+y < 7; }
}"
false

let%test "test_typecheck_decl_2" = test_typecheck 
"contract C {
    int x;
    function f(int y) public { require x+y < 7; }
    function g(bool b) public { require b; }
    function h(address a) public { }
    function f(bool b) public { require b; }
}"
false

let%test "test_typecheck_decl_3" = test_typecheck 
"contract C {
    int x;
    function f(int y) public { require x+y < 7; }
    function g(bool b) public { require b; }
    function h(address a) public { }
}"
true

let%test "test_typecheck_decl_4" = test_typecheck 
"contract C {
    int x;
    constructor(int y) { x=y; }
    function g(bool b) public { require b; }
    constructor() { x=1; }
}"
false

let%test "test_typecheck_decl_5" = test_typecheck 
"contract C {
    int x;
    function f(int b, address a, bool b) public { require b; }
}"
false

let%test "test_typecheck_decl_6" = test_typecheck 
"contract C {
    int x;
    function f(int x) public { }
}"
true

let%test "test_typecheck_decl_7" = test_typecheck 
"contract C {
    int x;
    function f(int x) public { int x; x = 1; }
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

let%test "test_typecheck_ife_1" = test_typecheck 
  "contract C {
      uint x;
      function f(int y,uint z) public { x = (y>5)?y:z; }
  }"
  false

let%test "test_typecheck_ife_2" = test_typecheck 
  "contract C {
      uint x;
      function f(int y,uint z) public { x = ((y>5)?uint(y):z) + 3; }
  }"
  true

let%test "test_typecheck_ife_3" = test_typecheck 
  "contract C {
      int x;
      function f(int y,uint z) public { x = (y>5)?y:z; }
  }"
  false

let%test "test_typecheck_ife_4" = test_typecheck 
  "contract C {
      int x;
      function f(int y,uint z) public { x = ((y>5)?y:int(z)) + 3; }
  }"
  true

let%test "test_typecheck_ife_5" = test_typecheck 
  "contract C {
      uint x;
      function f(uint y) public { x = ((y>5)?y:1) + 3; }
  }"
  true

let%test "test_typecheck_ife_6" = test_typecheck 
  "contract C {
      uint x;
      function f(uint y, int z) public { x = ((y>5)?y:1) + z; }
  }"
  false

let%test "test_typecheck_ife_7" = test_typecheck 
  "contract C {
      uint x;
      function f(uint y) public { x = 1 - ((y>5)?4:5); }
  }"
  false (* this is not type-checkable by solc *)

let%test "test_typecheck_ife_8" = test_typecheck 
  "contract C {
      uint x;
      function f(uint y) public { x = -4 + ((y>5)?4:5); }
  }"
  true (* this is not type-checkable by solc *)

let%test "test_typecheck_enum_1" = test_typecheck
  "contract C { enum State {IDLE,REQ} State s; function f() public { s = State.REQ; } }"
  true

let%test "test_typecheck_enum_2" = test_typecheck
  "contract C { enum State {IDLE,REQ} State s; function f() public { s = State.req; } }"
  false

let%test "test_typecheck_enum_3" = test_typecheck
  "contract C { enum State {IDLE,REQ} State s; function f() public { s = Stat.REQ; } }"
  false

let%test "test_typecheck_enum_4" = test_typecheck
  "contract C { enum State {IDLE,REQ,IDLE} State s; function f() public { s = State.REQ; } }"
  false

let%test "test_typecheck_enum_5" = test_typecheck
  "contract C { enum E1 {A1,B1} enum E2 {A2,B2} enum E3 {A1,B1} E1 s; function f() public { s = E1.A1; } }"
  true

let%test "test_typecheck_enum_6" = test_typecheck
  "contract C { enum E1 {A1,B1} enum E2 {A2,B2} enum E1 {A1,B1} E1 s; function f() public { s = E1.A1; } }"
  false