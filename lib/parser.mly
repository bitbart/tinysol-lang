%{
open Ast
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
%token LEQ
%token LE
%token GEQ
%token GE
%token <string> ID
%token <string> CONST
%token <string> STRING

%token SKIP
%token TAKES
%token CMDSEP
%token IF
%token ELSE
%token REQ

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token EOF

%token CONTRACT
%token CONSTR
%token FUN
%token INT
%token RECEIVESEP
%token SENDSEP
%token TOKSEP
%token ARGSEP

(* sequences are reduced eagerly *)
%left SKIP LBRACE ID WHILE IF ELSE DO
%nonassoc CMDSEP

%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left MUL

%start <contract> contract
%type <decls> decls
%type <cmd> cmd
%type <args> args
%type <expr> expr 

%start <cmd> cmd_test

%%

contract:
  | CONTRACT; c=ID; LBRACE; d = decls; RBRACE; EOF { Contract(c,d) }
;

expr:
  | n = CONST { IntConst(int_of_string n) }
  | s = STRING { AddrConst(s) }
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr { Not e }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e1=expr; PLUS; e2=expr { Add(e1,e2) }
  | e1=expr; MINUS; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | e1=expr; LE; e2=expr { Le(e1,e2) }
  | e1=expr; GEQ; e2=expr { Geq(e1,e2) }
  | e1=expr; GE; e2=expr { Ge(e1,e2) }
  | x = ID { Var(x) }
  | LPAREN; e = expr; RPAREN { e }
;

cmd:
  | SKIP { Skip }
  | REQ; e = expr { Req(e) } 
  | IF; e0 = expr; c1 = cmd; ELSE; c2 = cmd; { If(e0,c1,c2) }
  | x = ID; TAKES; e=expr; { Assign(x,e) }
  | x = ID; SENDSEP; e=expr; TOKSEP; t = ID; { Send(x,e,t) }
  | f = ID; LPAREN; e=expr; RPAREN { Call(f,e) }
  | c1 = cmd; CMDSEP; c2 = cmd; { Seq(c1,c2) } %prec CMDSEP
  | LBRACE; c = cmd; RBRACE { c }
;

cmd_test:
  | c = cmd; EOF { c }
;

args:
  | a = separated_list(ARGSEP, arg) { a } ;

arg:
  | x = ID { IntArg(x) }
  | RECEIVESEP; x = ID; TOKSEP; t = ID; { RcvArg(x,t) }
;

decls:
  | d = separated_list(CMDSEP, decl) { d } ;

decl:
  | INT; x = ID; { IntVar(x) }
  | CONSTR; f = ID; LPAREN; a = args; RPAREN; LBRACE; c = cmd; RBRACE { Constr(f,a,c) }
  | FUN; f = ID; LPAREN; a = args; RPAREN; LBRACE; c = cmd; RBRACE { Proc(f,a,c) }
;
