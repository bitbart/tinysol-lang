{
open Lexing
open Parser

exception LexerError of string

}

let white = [' ' '\n' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let newline = '\r' | '\n' | "\r\n"

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
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }  
  | "==" { EQ }
  | "<=" { LEQ }  
  | "<" { LE }
  | ">=" { GEQ }  
  | ">" { GE }
  | "?" { RECEIVESEP }
  | "!" { SENDSEP }
  | ":" { TOKSEP }
  | "contract" { CONTRACT }
  | "skip" { SKIP }
  | "="  { TAKES }
  | ";"  { CMDSEP }
  | "if" { IF }
  | "else" { ELSE }
  | "require" { REQ }
  | "constructor" { CONSTR } 
  | "function" { FUN }
  | "," { ARGSEP }  
  | "int" { INT }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
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
