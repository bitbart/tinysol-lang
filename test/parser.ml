open TinysolLib.Ast
open TinysolLib.Utils

(********************************************************************************
 test_parse_cmd : (command, expected AST)
 ********************************************************************************)

let test_parse_cmd (c : string) (t : cmd) =
  c
  |> parse_cmd
  |> blockify_cmd
  |> fun x -> x = t

let%test "test_parse_cmd_1" = test_parse_cmd
  "skip;" 
  Skip  

let%test "test_parse_cmd_2" = test_parse_cmd
  "x=51;"
  (Assign("x",IntConst 51))  

let%test "test_parse_cmd_3" = test_parse_cmd
  "{ int x; x=51; }" 
  (Block([{ ty=VarT(IntBT); name="x"; }],Assign("x",IntConst 51)))  

let%test "test_parse_cmd_4" = test_parse_cmd
  "{ int x; x=51; x=x+1; }"
  (Block([{ ty=VarT(IntBT); name="x"}],Seq(Assign("x",IntConst 51),Assign("x",Add(Var "x",IntConst 1)))))  

let%test "test_parse_cmd_5" = test_parse_cmd
  "{ int x; x=51; x=x+1; skip; }" 
  (Block([{ ty=VarT(IntBT); name="x"}],
    Seq(
      Assign("x",IntConst 51),
      Seq(
        Assign("x",Add(Var "x",IntConst 1)),
        Skip
      )
    )))

let%test "test_parse_cmd_6" = test_parse_cmd
  "{ uint x; mapping (uint => int) m; x = m[x+1]; }"
  (Block ([{ ty=VarT(UintBT); name="x" }; { ty=MapT (UintBT, IntBT); name="m" }],
  Assign ("x", MapR (Var "m", Add (Var "x", IntConst 1)))))

let%test "test_parse_cmd_7" = test_parse_cmd
  "{ mapping (uint => uint) m; m[0] = m[0]+1; }"
  (Block ([{ ty = MapT (UintBT, UintBT); name="m" }],
 MapW ("m", IntConst 0, Add (MapR (Var "m", IntConst 0), IntConst 1))))

let%test "test_parse_cmd_8" = test_parse_cmd
  "{ mapping (uint => uint) m; m[m[0]] = m[m[1]+2]+3; }"
  (Block ([{ ty=MapT (UintBT, UintBT); name="m" }],
 MapW ("m", MapR (Var "m", IntConst 0),
  Add (MapR (Var "m", Add (MapR (Var "m", IntConst 1), IntConst 2)),
   IntConst 3))))

let%test "test_parse_cmd_9" = test_parse_cmd
  "x = (true)?1:0;"
  (Assign ("x", IfE (BoolConst true, IntConst 1, IntConst 0)))

let%test "test_parse_cmd_10" = test_parse_cmd
  "x = (true)?(false)?0:1:2;"
  (Assign ("x", IfE (BoolConst true, IfE (BoolConst false, IntConst 0, IntConst 1), IntConst 2)))

let%test "test_parse_cmd_11" = test_parse_cmd
  "x = (true)?0:(false)?0:1;"
  (Assign ("x", IfE (BoolConst true, IntConst 0, IfE (BoolConst false, IntConst 0, IntConst 1))))

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
  (Contract ("C", [], [{ ty=VarT(UintBT); name="x"; visibility=Internal; immutable=false }],
  [Proc ("f", [], Assign ("x", BlockNum), Public, NonPayable, None)]))
  with _ -> false

let test_parse (c : string) (b : bool) = try 
  let _ = parse_contract c 
  in true
  with _ -> false
  |> fun x -> x = b

let%test "test_parse_contract_3" = test_parse 
  "contract C {
      int immutable x;
      function f(int y) public { y=x; }
  }"
  true

let%test "test_parse_contract_4" = test_parse
  "contract C {
      int x;
      function f(int immutable y) public { x=y; }
  }"
  false 

let%test "test_parse_contract_5" = test_parse
  "contract C { 
    address payable a; 
    function f() public payable { a.transfer(address(this).balance); } 
  }"
  true

let%test "test_parse_contract_6" = test_parse 
  "contract C { 
    function f(address payable a) public payable { a.transfer(address(this).balance); } 
  }"
  true

let%test "test_parse_contract_7" = test_parse 
  "contract C { enum State {IDLE, REQ} uint x; function f() public { x = x+1; } }"
  true

let%test "test_parse_contract_8" = test_parse
  "contract C { enum State {IDLE, REQ} State s; function f() public { s = State.REQ; } }"
  true

let%test "test_parse_contract_9" = test_parse
  "contract C { function f() public view payable { } }"
  false

let%test "test_parse_contract_10" = test_parse
  "contract C { function f() public view payable { } }"
  false

let%test "test_parse_contract_11" = test_parse
  "contract C { function f() public payable pure { } }"
  false

let%test "test_parse_contract_12" = test_parse
  "contract C { function f() payable public  { } }"
  true

let%test "test_parse_contract_13" = test_parse
  "contract C { function f() view public payable { } }"
  false

let%test "test_parse_contract_14" = test_parse
  "contract C { function f() view public { } }"
  true

let%test "test_parse_contract_15" = test_parse
  "contract C { function f() public { } }"
  true

let%test "test_parse_contract_16" = test_parse
  "contract C { function f() view payable { } }"
  false