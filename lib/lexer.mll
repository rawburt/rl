{
open Lexing
open Scanner

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
              pos_lnum = pos.pos_lnum + 1
    }
}

let newline = '\r' | '\n' | "\r\n"

rule read_token = parse
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { EQUAL }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | "==" { EQ }
  | "!=" { NEQ }
  | "!" { NOT }
  | "<" { LT }
  | "<=" { LE }
  | ">" { GT }
  | ">=" { GE }
  | "," { COMMA }
  | ":" { COLON }
  | "->" { ARROW }
  | "|" { PIPE }
  | "def" { DEF }
  | "do" { DO }
  | "end" { END }
  | "while" { WHILE }
  | "if" { IF }
  | "else" { ELSE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "type" { TYPE }
  | [' ' '\t']+ { read_token lexbuf }
  | "#" { read_comment lexbuf }
  | '-'? ['0'-'9']+ { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z'] (['a'-'z'] | '_')* { ID (Lexing.lexeme lexbuf) }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ { raise (SyntaxError ("[Lexer] illegal charcter: " ^ Lexing.lexeme lexbuf )) }

and read_comment = parse
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ { read_comment lexbuf }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
