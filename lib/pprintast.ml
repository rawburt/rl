open Ast
open Format

let pp_binop ppf = function
  | Plus_binop -> fprintf ppf "+"
  | Minus_binop -> fprintf ppf "-"
  | Times_binop -> fprintf ppf "*"
  | Divide_binop -> fprintf ppf "/"
  | Eq_binop -> fprintf ppf "=="
  | Neq_binop -> fprintf ppf "!="
  | Lt_binop -> fprintf ppf "<"
  | Le_binop -> fprintf ppf "<-"
  | Gt_binop -> fprintf ppf ">"
  | Ge_binop -> fprintf ppf ">="

let pp_unop ppf = function
  | Not_unop -> fprintf ppf "!"
  | Neg_unop -> fprintf ppf "-"

let rec pp_let_expr ppf name expr =
  fprintf ppf "@[%s = %a@]" name pp_expr expr

and pp_if_expr ppf cond_expr then_expr else_expr =
  fprintf ppf "@[<v>if %a do@;<0 2>@[<v 0>%a@]else@;<0 2>@[<v 0>%a@]@;end@]"
    pp_expr cond_expr
    pp_expr_list then_expr
    pp_expr_list else_expr

and pp_while_expr ppf cond_expr body_expr =
  fprintf ppf "@[<v>while %a do@;<0 2>@[<v 0>%a@]@;end@]"
    pp_expr cond_expr
    pp_expr_list body_expr

and pp_binop_expr ppf binop lhs_expr rhs_expr =
  fprintf ppf "%a %a %a"
    pp_expr lhs_expr
    pp_binop binop
    pp_expr rhs_expr

and pp_unop_expr ppf unop expr =
  fprintf ppf "%a %a" pp_unop unop pp_expr expr

and pp_call_expr ppf name args =
  fprintf ppf "%s(%a)" name pp_call_args args

and pp_record_expr ppf name args =
  fprintf ppf "%s(%a)" name pp_record_args args

and pp_record_args ppf = function
  | [] -> fprintf ppf ""
  | [(name, expr)] -> fprintf ppf "%s: %a" name pp_expr expr
  | (name, expr) :: args ->
      fprintf ppf "%s: %a, %a" name pp_expr expr pp_record_args args

and pp_call_args ppf = function
  | [] -> fprintf ppf ""
  | [arg] -> fprintf ppf "%a" pp_expr arg
  | arg :: args -> fprintf ppf "%a, %a" pp_expr arg pp_call_args args

and pp_expr_list ppf = function
  | [] -> fprintf ppf ""
  | exp :: [] -> fprintf ppf "%a" pp_expr exp
  | exp :: exps ->
      fprintf ppf "%a@;%a" pp_expr exp pp_expr_list exps

and pp_var_expr ppf = function
  | Simple_var (name, _) -> fprintf ppf "%s" name
  | Field_var (parent, name, _) ->
    fprintf ppf "%a.%s" pp_var_expr parent name

and pp_expr ppf = function
  | Bool_expr (b, _) -> fprintf ppf "%B" b
  | Number_expr (n, _) -> fprintf ppf "%d" n
  | String_expr (s, _) -> fprintf ppf "\"%s\"" s
  | Var_expr (v, _) -> pp_var_expr ppf v
  | Let_expr (name, expr, _) -> pp_let_expr ppf name expr
  | If_expr (cond_expr, then_expr, else_expr, _) ->
    pp_if_expr ppf cond_expr then_expr else_expr
  | While_expr (cond_expr, body_expr, _) ->
    pp_while_expr ppf cond_expr body_expr
  | Binop_expr (binop, lhs_expr, rhs_expr, _) ->
    pp_binop_expr ppf binop lhs_expr rhs_expr
  | Unop_expr (unop, expr, _) ->
    pp_unop_expr ppf unop expr
  | Call_expr (name, args, _) ->
    pp_call_expr ppf name args
  | Record_expr (name, args, _) ->
    pp_record_expr ppf name args

and pp_fields ppf = function
  | [] -> fprintf ppf ""
  | [field] -> fprintf ppf "%s : %s" field.field_name field.field_type
  | field :: fields ->
    fprintf ppf "%s : %s, %a"
      field.field_name
      field.field_type
      pp_fields fields

and pp_field_list ppf = function
  | [] -> fprintf ppf ""
  | fields -> fprintf ppf "(%a)" pp_fields fields

let pp_fundecl_result ppf = function
  | "unit" -> fprintf ppf ""
  | result -> fprintf ppf " -> %s" result

let pp_fundecl ppf f =
  fprintf ppf "@[<v>def %s%a%a do@;<0 2>@[<v 0>%a@]@;end@]"
    f.fundecl_name
    pp_field_list f.fundecl_params
    pp_fundecl_result f.fundecl_result
    pp_expr_list f.fundecl_body

let pp_recdecl ppf r =
  fprintf ppf "record %s%a"
    r.recdecl_name
    pp_field_list r.recdecl_params

let rec pp_decl_list ppf = function
  | [] -> ()
  | decl :: decls ->
    match decl with
    | Fun_decl f ->
      fprintf ppf "%a@;@." pp_fundecl f;
      pp_decl_list ppf decls
    | Record_decl r ->
      fprintf ppf "%a@;@." pp_recdecl r;
      pp_decl_list ppf decls

let print_expr =
  let pp ppf exp = fprintf ppf "%a@;@." pp_expr exp in
  pp std_formatter

let print_program =
  let pp ppf decls = pp_decl_list ppf decls in
  pp std_formatter
