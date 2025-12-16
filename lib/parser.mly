%{
open Ast
open Cli_ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token PLUS
%token MINUS
%token MUL
%token EQ
%token NEQ
%token LEQ
%token LT
%token GEQ
%token GT
%token DOT
%token <string> ID
%token <string> CONST
%token <string> STRING
%token <string> ADDRLIT

%token SKIP
%token TAKES
%token ADDTAKES
%token SUBTAKES
%token CMDSEP
%token IF
%token ELSE
%token REQ

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LSQUARE
%token RSQUARE
%token MAPSTO
%token MAPPING
%token ENUM
%token EOF

%token CONTRACT
%token CONSTR
%token FUN
%token RECEIVE
%token RETURN
%token INT
%token UINT
%token BOOL
%token ADDR
%token THIS
%token MSGSENDER
%token MSGVALUE
%token BALANCE
%token TRANSFER
%token COLON
%token QMARK
%token VALUE
%token ARGSEP
%token PUBLIC
%token PRIVATE
%token INTERNAL
%token EXTERNAL
%token VIEW
%token PURE
%token PAYABLE
%token IMMUTABLE
%token CONSTANT
%token RETURNS 

%token FAUCET
%token DEPLOY
%token ASSERT
%token LASTREVERTED
%token BLOCKNUM

%token PRAGMA
%token SOLIDITY
%token CARET
%token <string> VERSION

%left OR
%left AND
%nonassoc NOT
%nonassoc EQ NEQ LEQ GEQ LT GT
%left PLUS MINUS
%nonassoc UMINUS
%left MUL

%left CMDSEP
%nonassoc ELSE

%start <contract> contract
%type <exprval> value
%type <var_decl> var_decl
%type <local_var_decl> local_var_decl
%type <visibility_t> visibility_t
%type <fun_decl> fun_decl
%type <cmd> cmd
%type <local_var_decl list> formal_args
%type <expr> expr 

%start <cmd> cmd_eof
%start <expr> expr_eof

%start <transaction> transaction
%start <cli_cmd> cli_cmd


%%

contract:
  | opt_pragma; CONTRACT; c=ID; LBRACE; el = list(enum_decl); vdl = contract_vars; fdl = list(fun_decl); RBRACE; EOF { Contract(c,el,vdl,fdl) }
;

rel_version:
  | GEQ {}
  | LEQ {}
  | TAKES {}

opt_pragma:
  | PRAGMA; SOLIDITY; rel_version; VERSION; CMDSEP { }
  | PRAGMA; SOLIDITY; CARET; VERSION; CMDSEP { }
  | /* empty */ { }

contract_vars:
  | vd = var_decl; CMDSEP; l = contract_vars { vd::l }
  | /* empty */ { [] }

enum_decl:
  | ENUM; x = ID; LBRACE; ol = separated_list(ARGSEP, ID); RBRACE { Enum(x,ol) }; 

actual_args:
  | a = separated_list(ARGSEP, value) { a } ;

value:
  | n = CONST { Int (int_of_string n) }
  | s = STRING { Addr s }
  | TRUE { Bool true }
  | FALSE { Bool false }

opt_weivalue:
  | LBRACE; VALUE; COLON; e_value = expr; RBRACE; { e_value }
  | { IntConst 0 }

expr:
  | n = CONST { IntConst(int_of_string n) }
  | s = STRING { AddrConst(s) }
  | THIS { This }
  | MSGSENDER { Var("msg.sender") }
  | MSGVALUE { Var("msg.value") }
  | e = expr; DOT; BALANCE { BalanceOf(e) }
  | e = expr; DOT; o = ID { match e with Var(x) -> EnumOpt(x,o) | _ -> failwith "enum parser error"}
  | e1 = expr; LPAREN; e2 = expr; RPAREN { match e1 with Var(x) -> UnknownCast(x,e2) | _ -> failwith "enum parser error"}
  | e_to = expr; DOT; f = ID; e_value = opt_weivalue; LPAREN; e_args = separated_list(ARGSEP, expr); RPAREN { FunCall(e_to,f,e_value,e_args) }
  | TRUE { BoolConst true }
  | FALSE { BoolConst false }
  | BLOCKNUM { BlockNum }
  | NOT; e=expr { Not e }
  | MINUS; e=expr %prec UMINUS { Sub(IntConst 0, e) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e1=expr; PLUS; e2=expr { Add(e1,e2) }
  | e1=expr; MINUS; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; NEQ; e2=expr { Neq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | e1=expr; LT; e2=expr { Lt(e1,e2) }
  | e1=expr; GEQ; e2=expr { Geq(e1,e2) }
  | e1=expr; GT; e2=expr { Gt(e1,e2) }
  | e1=expr; LSQUARE; e2=expr; RSQUARE { MapR(e1,e2) }
  | LPAREN; e1 = expr; RPAREN; QMARK; e2 = expr; COLON; e3 = expr { IfE(e1,e2,e3) }
  | INT; LPAREN; e=expr; RPAREN; { IntCast(e) }
  | UINT; LPAREN; e=expr; RPAREN; { UintCast(e) }
  | ADDR; LPAREN; e=expr; RPAREN; { AddrCast(e) }
  | PAYABLE; LPAREN; e=expr; RPAREN; { PayableCast(e) }
  | x = ID { Var(x) }
  | LPAREN; e = expr; RPAREN { e }
;

expr_eof:
  | e = expr; EOF { e }
;

base_type:
  | INT  { IntBT  }
  | UINT { UintBT }
  | BOOL { BoolBT }
  | ADDR; p = opt_payable { AddrBT(p) }

opt_id:
  | ID { }
  | /* empty */ { }

var_type:
  | t = base_type; { VarT(t) }
  | MAPPING; LPAREN; t1 = base_type; opt_id; MAPSTO; t2 = base_type; opt_id; RPAREN { MapT(t1,t2) }

visibility_t:
  | PUBLIC    { Public }
  | PRIVATE   { Private }
  | INTERNAL  { Internal }
  | EXTERNAL  { External }
;

opt_var_visibility_t:
  | v = visibility_t { v }
  | /* default */ { Internal }

opt_var_mutability_t:
  | CONSTANT { Constant }
  | IMMUTABLE { Immutable }
  | /* default */ { Mutable }

opt_var_modifiers:
  | v = opt_var_visibility_t; m = opt_var_mutability_t { (v,m) }
  | m = opt_var_mutability_t v = opt_var_visibility_t; { (v,m) }

opt_init_value:
  | TAKES; v = value; { Some v }
  | /* default */ { None }
  
var_decl:
  | t = var_type; v_i = opt_var_modifiers; x = ID; iv = opt_init_value { { ty = t; name = x; visibility = fst v_i; mutability = snd v_i; init_value = iv } }
  | t = ID; v_i = opt_var_modifiers; x = ID { { ty = VarT(UnknownBT(t)); name = x; visibility = fst v_i; mutability = snd v_i; init_value = None } }
;

local_var_decl:
  | t = var_type; x = ID { { ty = t; name = x; } }
  | t = ID; x = ID { { ty = VarT(UnknownBT(t)); name = x; } }
;

opt_id_decons:
  | x = ID; { Some x }
  | /* empty */ { None }

nonseq_cmd:
  | SKIP; CMDSEP;  { Skip }
  | REQ; e = expr; CMDSEP; { Req(e) } 
  | RETURN; LPAREN; el = separated_nonempty_list(ARGSEP, expr); RPAREN; CMDSEP; { Return(el) } 
  | RETURN; e = expr; CMDSEP; { Return([e]) } 
  | x = ID; TAKES; e = expr; CMDSEP; { Assign(x,e) }
  | x = ID; ADDTAKES; e = expr; CMDSEP; { Assign(x,Add(Var(x),e)) }
  | x = ID; SUBTAKES; e = expr; CMDSEP; { Assign(x,Sub(Var(x),e)) }
  | x = ID; LSQUARE; ek = expr; RSQUARE; TAKES; ev = expr; CMDSEP; { MapW(x,ek,ev) }
  | x = ID; LSQUARE; ek = expr; RSQUARE; ADDTAKES; ev = expr; CMDSEP; { MapW(x,ek,Add(MapR(Var x,ek),ev)) }
  | x = ID; LSQUARE; ek = expr; RSQUARE; SUBTAKES; ev = expr; CMDSEP; { MapW(x,ek,Sub(MapR(Var x,ek),ev)) }
  | rcv = expr; DOT; TRANSFER; LPAREN; amt=expr; RPAREN; CMDSEP; { Send(rcv,amt) }
  | vd = local_var_decl; CMDSEP; { Decl(vd) }
  | e_to = expr; DOT; f = ID; e_value = opt_weivalue; LPAREN; e_args = separated_list(ARGSEP, expr); RPAREN; CMDSEP; { ProcCall(e_to,f,e_value,e_args) }
  | LPAREN; xl = separated_nonempty_list(ARGSEP,opt_id_decons); RPAREN; TAKES; e = expr; CMDSEP; { Decons(xl,e) }
;

cmd:
  | c = nonseq_cmd { c }
  | c1 = cmd; c2 = cmd { Seq(c1,c2) }
  | IF LPAREN; e = expr; RPAREN; c1 = nonseq_cmd ELSE c2 = nonseq_cmd { If(e,c1,c2) }
  | IF LPAREN; e = expr; RPAREN; LBRACE; c1 = cmd; RBRACE; ELSE c2 = nonseq_cmd { If(e,c1,c2) }
  | IF LPAREN; e = expr; RPAREN; c1 = nonseq_cmd; ELSE LBRACE; c2 = cmd; RBRACE; { If(e,c1,c2) }
  | IF LPAREN; e = expr; RPAREN; LBRACE; c1 = cmd; RBRACE; ELSE; LBRACE; c2 = cmd; RBRACE { If(e,c1,c2) }
  | IF LPAREN; e = expr; RPAREN; c1 = cmd { If(e,c1,Skip) }
  | LBRACE; c = cmd; RBRACE { Block([], c) }
;

cmd_eof:
  | c = cmd; EOF { c }
;

opt_returns:
  | RETURNS; LPAREN; tl = separated_list(ARGSEP, base_type); RPAREN { tl }
  | /* no return */ { [] }

opt_cmd:
  | c = cmd { c}
  | { Skip }

opt_fun_mutability_t:
  | PURE { Pure }
  | VIEW { View }
  | PAYABLE { Payable }
  | /* default */ { NonPayable }

opt_payable:
  | PAYABLE { true }
  | /* empty */ { false }

fun_modifiers:
  | v = visibility_t; m = opt_fun_mutability_t { (v,m) }
  | m = opt_fun_mutability_t; v = visibility_t { (v,m) }

fun_decl:
  /* constructor(al) payable? { c } */ 
  | CONSTR; LPAREN; al = formal_args; RPAREN; m = opt_fun_mutability_t; LBRACE; c = opt_cmd; RBRACE { Constr(al,c,m) }
  /* function f(al) [public|private]? payable? returns(r)? { c } */
  | FUN; f = ID; LPAREN; al = formal_args; RPAREN; fmod = fun_modifiers; ret = opt_returns; LBRACE; c = opt_cmd; RBRACE { Proc(f,al,c,fst fmod,snd fmod,ret) }
  | RECEIVE; LPAREN; al = formal_args; RPAREN; fmod = fun_modifiers; ret = opt_returns; LBRACE; c = opt_cmd; RBRACE { Proc("receive",al,c,fst fmod,snd fmod,ret) }
;

formal_args:
  | a = separated_list(ARGSEP, formal_arg) { a } ;

formal_arg:
  | INT;  x = ID { { ty = VarT(IntBT); name = x; } }
  | UINT; x = ID { { ty = VarT(UintBT); name = x; } }
  | BOOL; x = ID { { ty = VarT(BoolBT); name = x; } }
  | ADDR; p = opt_payable; x = ID { { ty = VarT(AddrBT(p)); name = x; } }
;

opt_weivalue_tx:
  | LBRACE; VALUE; COLON; n = CONST; RBRACE; { int_of_string n }
  | { 0 }

transaction:
  | s = ADDRLIT; COLON; c = ADDRLIT; v = opt_weivalue_tx; LPAREN; al = actual_args; RPAREN 
  { { txsender = s;
      txto = c;
      txfun = "constructor";
      txargs = al;
      txvalue = v;
  } }
  | s = ADDRLIT; COLON; c = ADDRLIT; DOT; f = ID; v = opt_weivalue_tx; LPAREN; al = actual_args; RPAREN 
  { { txsender = s;
      txto = c;
      txfun = f;
      txargs = al;
      txvalue = v;
  } }
  | s = ADDRLIT; COLON; c = ADDRLIT; DOT; RECEIVE; v = opt_weivalue_tx; LPAREN; al = actual_args; RPAREN 
  { { txsender = s;
      txto = c;
      txfun = "receive";
      txargs = al;
      txvalue = v;
  } }
;

cli_cmd:
  | tx = transaction { CallFun tx }
  | FAUCET; a = ADDRLIT; n = CONST { Faucet(a, int_of_string n) }
  | DEPLOY; tx = transaction; filename = STRING { Deploy(tx,filename) }
  | ASSERT; a = ADDRLIT; e = expr; EOF; { Assert(a,e) }
  | ASSERT; LASTREVERTED { LastReverted }
  | ASSERT; NOT; LASTREVERTED { NotLastReverted }
  | BLOCKNUM; TAKES; n = CONST; { SetBlockNum(int_of_string n) }
;