{
open Lexing
open Parser

exception LexerError of string
}

let white = [' ' '\n' '\t']+
let letter_or_underscore = ['a'-'z' 'A'-'Z' '_']
let chr = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let id = letter_or_underscore chr*
let addrlit = '0' 'x' chr+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let newline = '\r' | '\n' | "\r\n"
let version = ['0'-'9']'.'['0'-'9']('.'['0'-'9'])?

rule read_token =
  parse
  | white { read_token lexbuf }
  | "//" { read_single_line_comment lexbuf }
  | "/*" { read_multi_line_comment lexbuf }
  | '"'  { read_string (Buffer.create 17) lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LSQUARE }
  | "]" { RSQUARE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "!" { NOT }
  | "&&" { AND }
  | "||" { OR }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }  
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LEQ }  
  | "<" { LT }
  | ">=" { GEQ }  
  | ">" { GT }
  | "=>" { MAPSTO }
  | "this" { THIS }
  | "msg.sender" { MSGSENDER }
  | "msg.value" { MSGVALUE }
  | "balance" { BALANCE }
  | "?" { QMARK }
  | ":" { COLON }
  | "value" { VALUE }
  | "contract" { CONTRACT }
  | "skip" { SKIP }
  | "transfer" { TRANSFER }
  | "="  { TAKES }
  | "+="  { ADDTAKES }
  | "-="  { SUBTAKES }
  | ";"  { CMDSEP }
  | "." { DOT }
  | "if" { IF }
  | "else" { ELSE }
  | "require" { REQ }
  | "return" { RETURN }
  | "constructor" { CONSTR } 
  | "function" { FUN }
  | "receive" { RECEIVE }
  | "," { ARGSEP }  
  | "int" { INT }
  | "uint" { UINT }
  | "bool" { BOOL }
  | "address" { ADDR }
  | "mapping" { MAPPING }
  | "enum" { ENUM }
  | "public" { PUBLIC }
  | "private" { PRIVATE }
  | "internal" { INTERNAL }
  | "external" { EXTERNAL }
  | "view" { VIEW }
  | "pure" { PURE }
  | "payable" { PAYABLE }
  | "immutable" { IMMUTABLE }
  | "constant" { CONSTANT }
  | "returns" { RETURNS }
  | "faucet" { FAUCET }
  | "deploy" { DEPLOY }
  | "assert" { ASSERT }
  | "lastReverted" { LASTREVERTED }
  | "block.number" { BLOCKNUM }
  | "pragma" { PRAGMA }
  | "^" { CARET }
  | "solidity" { SOLIDITY }
  | id { ID (Lexing.lexeme lexbuf) }
  | addrlit { ADDRLIT (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | version { VERSION (Lexing.lexeme lexbuf) }
  | newline { new_line lexbuf; read_token lexbuf }  
  | eof { EOF }

and read_single_line_comment = parse
  | newline { new_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }

and read_multi_line_comment = parse
  | "*/" { read_token lexbuf }
  | newline { new_line lexbuf; read_multi_line_comment lexbuf }
  | eof { raise End_of_file }
  | _ { read_multi_line_comment lexbuf }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof { raise End_of_file }
  | _ { raise (LexerError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
