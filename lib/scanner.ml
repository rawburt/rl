open Ast

type token =
  | LPAREN
  | RPAREN
  | EQUAL
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQ
  | NEQ
  | NOT
  | LT
  | LE
  | GT
  | GE
  | COMMA
  | COLON
  | ARROW
  | PIPE
  | DEF
  | DO
  | END
  | WHILE
  | IF
  | ELSE
  | TRUE
  | FALSE
  | TYPE
  | EOF
  | NUMBER of int
  | STRING of string
  | ID of string

let string_of_token = function
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | EQUAL -> "EQUAL"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIVIDE -> "DIVIDE"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | NOT -> "NOT"
  | LT -> "LT"
  | LE -> "LE"
  | GT -> "GT"
  | GE -> "GE"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | ARROW -> "ARROW"
  | PIPE -> "PIPE"
  | DEF -> "DEF"
  | DO -> "DO"
  | END -> "END"
  | WHILE -> "WHILE"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | TYPE -> "TYE"
  | EOF -> "EOF"
  | NUMBER n -> "NUMBER " ^ string_of_int n
  | STRING s -> "STRING \"" ^ s ^ "\""
  | ID id -> "ID " ^ id

let tok_prec = function
  | EQ -> 10
  | NEQ -> 10
  | LT -> 10
  | LE -> 10
  | GT -> 10
  | GE -> 10
  | PLUS -> 20
  | MINUS -> 20
  | TIMES -> 40
  | DIVIDE -> 40
  | _ -> 0

let is_binop = function
  | EQ
  | NEQ
  | LT
  | LE
  | GT
  | GE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE -> true
  | _ -> false

let is_right_assoc = function
  | EQ | NEQ -> true
  | _ -> false

type ctx = {
  lexbuf : Lexing.lexbuf ;
  read_token : (Lexing.lexbuf -> token) ;
  mutable peek : token option ;
  debug : bool
}

exception ParseError of string * pos
exception NotImplemented

let error_unexpected_token expected got pos =
  let expected_s = string_of_token expected in
  let got_s = string_of_token got in
  let message = Printf.sprintf "unexpected token: %s | expected: %s" got_s expected_s in
  raise (ParseError (message, pos))

let error_expected_ident got pos =
  let got_s = string_of_token got in
  let message = Printf.sprintf "expected identifier but got: %s" got_s in
  raise (ParseError (message, pos))

let error_expected_decl got pos =
  let got_s = string_of_token got in
  let message = Printf.sprintf "expected top level declaration but got: %s" got_s in
  raise (ParseError (message, pos))

let error_expected_statement got pos =
  let got_s = string_of_token got in
  let message = Printf.sprintf "expected a statement but got: %s" got_s in
  raise (ParseError (message, pos))

let error_expected_expression pos =
  let message = "expected expression but none was found" in
  raise (ParseError (message, pos))

let debug ctx msg = if ctx.debug then print_endline msg

let peek ctx =
  match ctx.peek with
  | Some token ->
    debug ctx ("peek (cached): " ^ string_of_token token);
    token
  | None ->
    let token = ctx.read_token ctx.lexbuf in
    ctx.peek <- Some token;
    debug ctx ("peek: " ^ string_of_token token);
    token

let eat_peek ctx =
  debug ctx "eat_peek";
  ctx.peek <- None

let advance ctx =
  match ctx.peek with
  | Some token ->
    debug ctx ("advance (from peek): " ^ string_of_token token);
    ctx.peek <- None;
    token
  | None ->
    let token = ctx.read_token ctx.lexbuf in
    debug ctx ("advance: " ^ string_of_token token);
    token

let pos ctx = ctx.lexbuf.lex_start_p

let expect_id ctx =
  debug ctx "expect_id";
  match advance ctx with
  | ID id -> id
  | t -> error_expected_ident t (pos ctx)

let expect ctx token =
  debug ctx ("expect " ^ string_of_token token);
  let t = advance ctx in
  if t <> token then error_unexpected_token token t (pos ctx)

let binop_from_token ctx = function
  | PLUS -> Plus_binop
  | MINUS -> Minus_binop
  | TIMES -> Times_binop
  | DIVIDE -> Divide_binop
  | EQ -> Eq_binop
  | NEQ -> Neq_binop
  | LT -> Lt_binop
  | LE -> Le_binop
  | GT -> Gt_binop
  | GE -> Ge_binop
  | t -> raise (ParseError (("expected binary operator but got: " ^ string_of_token t), pos ctx))

let binop_expression ctx op lhs rhs =
  debug ctx "binop_expression";
  let binop = binop_from_token ctx op in
  Binop_expr (binop, lhs, rhs, pos ctx)

(* expression parsing using operator-presedence parser mixed with recursive descent *)
(* ref: https://en.wikipedia.org/wiki/Operator-precedence_parser *)

let rec maybe_expression_list ctx =
  let rec expression_list es =
    match parse_expression ctx with
    | None -> raise (ParseError ("expected another expression after comma", pos ctx))
    | Some e ->
      match peek ctx with
      | COMMA ->
        eat_peek ctx;
        expression_list (e :: es)
      | _ -> List.rev es
  in
  match parse_expression ctx with
  | None -> []
  | Some e ->
    match peek ctx with
    | COMMA ->
      eat_peek ctx;
      expression_list [e]
    | _ -> [e]

and expression_call name ctx =
  debug ctx "expression_call";
  let cpos = pos ctx in
  expect ctx LPAREN;
  let args = maybe_expression_list ctx in
  expect ctx RPAREN;
  Call_expr (name, args, cpos)

and base_expression ctx =
  debug ctx "expression";
  match peek ctx with
  | ID name ->
    eat_peek ctx;
    begin
      match peek ctx with
      | LPAREN -> Some (expression_call name ctx)
      | _ -> Some (Var_expr (name, pos ctx))
    end
  | STRING s ->
    eat_peek ctx;
    Some (String_expr (s, pos ctx))
  | NUMBER n ->
    eat_peek ctx;
    Some (Number_expr (n, pos ctx))
  | _ -> None

and expect_expression ctx =
  debug ctx "expect_expression";
  match base_expression ctx with
  | Some e -> e
  | None -> raise (ParseError ("expected expression", pos ctx))

and parse_expression_1 ctx init_lhs min_prec =
  debug ctx "parse_expression_1";
  let lhs = ref init_lhs in
  let lookahead = ref (peek ctx) in
  let continue_main tok = is_binop tok && tok_prec tok >= min_prec in
  let continue_sub op tok =
    let opprec = tok_prec op in
    let tokprec = tok_prec tok in
    is_binop tok && (tokprec > opprec || (is_right_assoc tok && tokprec = opprec))
  in
  while continue_main !lookahead do
    debug ctx "parse_expression_1.main";
    let op = !lookahead in
    eat_peek ctx;
    let rhs = ref (expect_expression ctx) in
    lookahead := peek ctx;
    while continue_sub op !lookahead do
      debug ctx "parse_expression_1.main.sub";
      let opprec = tok_prec op in
      let modifier = if tok_prec !lookahead > opprec then 1 else 0 in
      let newprec = opprec + modifier in
      rhs := parse_expression_1 ctx !rhs newprec;
      lookahead := peek ctx
    done;
    lhs := binop_expression ctx op !lhs !rhs
  done;
  !lhs

(* main expression parser entry *)
and parse_expression ctx =
  debug ctx "parse_expression";
  match base_expression ctx with
  | None -> None
  | Some lhs -> Some (parse_expression_1 ctx lhs 0)

let assignment_statement name ctx =
  debug ctx "assignment_statement";
  expect ctx EQUAL;
  match parse_expression ctx with
  | None -> error_expected_expression (pos ctx)
  | Some expr -> Let_expr (name, expr, pos ctx)

let rec while_statement ctx =
  debug ctx "while_statement";
  let wpos = pos ctx in
  expect ctx WHILE;
  match parse_expression ctx with
  | None -> error_expected_expression (pos ctx)
  | Some cond ->
    expect ctx DO;
    let stmt_list = statement_list ctx in
    expect ctx END;
    While_expr (cond, stmt_list, wpos)

and statement ctx =
  debug ctx "statement";
  match peek ctx with
  | ID name ->
    eat_peek ctx;
    begin
      match peek ctx with
      | LPAREN -> Some (expression_call name ctx)
      | EQUAL -> Some (assignment_statement name ctx)
      | _ -> Some (Var_expr (name, pos ctx))
    end
  | WHILE -> Some (while_statement ctx)
  | _ -> None

and statement_list ctx =
  debug ctx "statement_list";
  let rec statements l =
    match statement ctx with
    | Some s -> statements (s :: l)
    | None -> List.rev l
  in
  statements []

let param_field ctx =
  debug ctx "param_field";
  let field_pos = pos ctx in
  let field_name = expect_id ctx in
  expect ctx COLON;
  let field_type = expect_id ctx in
  { field_name ; field_type ; field_pos }

let fun_decl_params ctx =
  debug ctx "fun_decl_params";
  let rec fields f =
    match peek ctx with
    | COMMA -> begin
      eat_peek ctx;
      let field = param_field ctx in
      fields (field :: f)
    end
    | _ -> List.rev f
  in
  fields [param_field ctx]

let maybe_fun_decl_params ctx =
  debug ctx "maybe_fun_decl_params";
  match peek ctx with
  | LPAREN ->
    eat_peek ctx;
    let fparams = fun_decl_params ctx in
    expect ctx RPAREN;
    Some fparams
  | _ -> None

let maybe_fun_decl_result ctx =
  debug ctx "maybe_fun_decl_result";
  match peek ctx with
  | ARROW ->
    eat_peek ctx;
    Some (expect_id ctx)
  | _ -> None

let scan_fun_decl ctx =
  debug ctx "scan_fun_decl";
  expect ctx DEF;
  let fpos = pos ctx in
  let fname = expect_id ctx in
  let fparams = maybe_fun_decl_params ctx in
  let fresult = maybe_fun_decl_result ctx in
  expect ctx DO;
  let fbody = statement_list ctx in
  expect ctx END;
  Fun_decl {
    fundecl_name = fname ;
    fundecl_params = Option.value fparams ~default:[] ;
    fundecl_result = Option.value fresult ~default:"unit" ;
    fundecl_body = fbody ;
    fundecl_pos = fpos ;
  }

let scan_type_decl _ctx = raise NotImplemented

let scan_program read_token lexbuf cfg_debug =
  let ctx = { lexbuf ; read_token ; peek = None ; debug = cfg_debug } in
  let rec program () =
    debug ctx "scan_program.program";
    match peek ctx with
    | EOF -> []
    | DEF ->
      let fdec = scan_fun_decl ctx in
      fdec :: (program ())
    | TYPE ->
      let tdec = scan_type_decl ctx in
      tdec :: (program ())
    | t -> error_expected_decl t (pos ctx)
  in
  program ()
