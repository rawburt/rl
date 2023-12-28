(* let print_error_position (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_file file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel ~with_positions:true chan in
  Lexing.set_filename lexbuf file;
  try Ok(Parser.program Lexer.read_token lexbuf) with
  | Lexer.SyntaxError msg -> Error (Printf.sprintf "%s | %s" (print_error_position lexbuf) msg)
  | Parser.Error -> Error (Printf.sprintf "[Parse Error]: %s" (print_error_position lexbuf))
   *)

let parse_file ?(debug = true) file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel ~with_positions:true chan in
  Lexing.set_filename lexbuf file;
  Scanner.scan_program Lexer.read_token lexbuf debug
