open Ast
open Env
open Types

exception TypeCheckError of string * pos
exception NotImplemented

let error_type_not_found expected pos =
  let message = Printf.sprintf "unknown type: %s" expected in
  raise (TypeCheckError (message, pos))

let error_type_mismatch expected got pos =
  let expected_s = string_of_type expected in
  let got_s = string_of_type got in
  let message = Printf.sprintf "expected type: %s | got type: %s" expected_s got_s in
  raise (TypeCheckError (message, pos))

let error_symbol_not_found name pos =
  let message = Printf.sprintf "symbol not found: %s" name in
  raise (TypeCheckError (message, pos))

let error_let_mutate_type_mismatch name expected got pos =
  let expected_s = string_of_type expected in
  let got_s = string_of_type got in
  let message = Printf.sprintf "symbol '%s' already of type '%s' but tried to mutate to '%s'" name expected_s got_s in
  raise (TypeCheckError (message, pos))

let error_let_function_def name pos =
  let message = Printf.sprintf "symbol '%s' is defined as a function and cant be mutated" name in
  raise (TypeCheckError (message, pos))

let error_first_class_function name pos =
  let message = Printf.sprintf "function '%s' must be invoked with parenthesis" name in
  raise (TypeCheckError (message, pos))

let error_while_cond got pos =
  let got_s = string_of_type got in
  let message = Printf.sprintf "while loop condition must be of type 'bool' but got: %s" got_s in
  raise (TypeCheckError (message, pos))

let error_if_cond got pos =
  let got_s = string_of_type got in
  let message = Printf.sprintf "if condition must be of type 'bool' but got: %s" got_s in
  raise (TypeCheckError (message, pos))

let error_if_mismatch then_ty else_ty pos =
  let then_s = string_of_type then_ty in
  let else_s = string_of_type else_ty in
  let message = Printf.sprintf "branches of an if statement must have the same type. then: %s | else: %s" then_s else_s in
  raise (TypeCheckError (message, pos))

let error_binop_type_mismatch binop lhs rhs pos =
  let binop_s = string_of_binop binop in
  let lhs_s = string_of_type lhs in
  let rhs_s = string_of_type rhs in
  let message = Printf.sprintf "operator '%s' given mismatched types. lhs: %s | rhs: %s" binop_s lhs_s rhs_s in
  raise (TypeCheckError (message, pos))

let error_binop_args binop expected_ty given_ty pos =
  let binop_s = string_of_binop binop in
  let expected_ty_s = string_of_type expected_ty in
  let given_ty_s = string_of_type given_ty in
  let message = Printf.sprintf "operator '%s' expected argument types of '%s' but was given '%s'" binop_s expected_ty_s given_ty_s in
  raise (TypeCheckError (message, pos))

let error_unop unop expected given pos =
  let unop_s = string_of_unop unop in
  let expected_s = string_of_type expected in
  let given_s = string_of_type given in
  let message = Printf.sprintf "operator '%s' expected type of '%s' but was given '%s'" unop_s expected_s given_s in
  raise (TypeCheckError (message, pos))

let error_call_var name pos =
  let message = Printf.sprintf "symbol '%s' is not a function and cant be called" name in
  raise (TypeCheckError (message, pos))

let error_wrong_number_args name pos =
  let message = Printf.sprintf "wrong number of args given to function '%s'" name in
  raise (TypeCheckError (message, pos))

let error_wrong_arg_types name pos =
  let message = Printf.sprintf "wrong arg types given to function '%s'" name in
  raise (TypeCheckError (message, pos))

let binop_math_ty tenv venv binop lhs_ty pos =
  if lhs_ty <> Number_ty
    then error_binop_args binop Number_ty lhs_ty pos
    else (tenv, venv, Number_ty)

let binop_compare_ty tenv venv binop lhs_ty pos =
  if lhs_ty <> Number_ty
    then error_binop_args binop Number_ty lhs_ty pos
    else (tenv, venv, Bool_ty)

(* NOTE: blocks are scopes and environment changes do not escape *)

let rec expr_ty tenv venv = function
  | Bool_expr (_, _) -> (tenv, venv, Bool_ty)
  | Number_expr (_, _) -> (tenv, venv, Number_ty)
  | String_expr (_, _) -> (tenv, venv, String_ty)
  | Var_expr (name, pos) -> begin
    match SymbolTable.find_opt name venv with
    | Some (Var_entry ty) -> (tenv, venv, ty)
    | Some (Fun_entry _) -> error_first_class_function name pos
    | None -> error_symbol_not_found name pos
  end
  | Let_expr (name, expr, pos) ->
    let (_, _, expr_ty) = expr_ty tenv venv expr in begin
    match SymbolTable.find_opt name venv with
    | Some (Var_entry ty) ->
      if expr_ty <> ty
        then error_let_mutate_type_mismatch name ty expr_ty pos
        else (tenv, venv, ty)
    | Some (Fun_entry _) -> error_let_function_def name pos
    | None ->
      let venv = SymbolTable.add name (Var_entry expr_ty) venv in
      (tenv, venv, expr_ty)
  end
  | While_expr (cond_expr, body_expr_list, pos) ->
    let (_, _, cond_expr_ty) = expr_ty tenv venv cond_expr in
    if cond_expr_ty <> Bool_ty
      then error_while_cond cond_expr_ty pos
      else
        expr_list_ty tenv venv body_expr_list
  | If_expr (cond_expr, then_expr, else_expr, pos) ->
    let (_, _, cond_expr_ty) = expr_ty tenv venv cond_expr in
    if cond_expr_ty <> Bool_ty
      then error_if_cond cond_expr_ty pos
      else
        let (_, _, then_ty) = expr_list_ty tenv venv then_expr in
        let (_, _, else_ty) = expr_list_ty tenv venv else_expr in
        if then_ty <> else_ty
          then error_if_mismatch then_ty else_ty pos
          else (tenv, venv, then_ty)
  | Binop_expr (binop, lhs, rhs, pos) ->
    let (_, _, lhs_ty) = expr_ty tenv venv lhs in
    let (_, _, rhs_ty) = expr_ty tenv venv rhs in
    if lhs_ty <> rhs_ty
      then error_binop_type_mismatch binop lhs_ty rhs_ty pos
      else begin
       match binop with
      | Plus_binop
      | Minus_binop
      | Times_binop
      | Divide_binop -> binop_math_ty tenv venv binop lhs_ty pos
      | Lt_binop
      | Le_binop
      | Gt_binop
      | Ge_binop -> binop_compare_ty tenv venv binop lhs_ty pos
      | Eq_binop
      | Neq_binop ->
        (* lhs = rhs was already checked *)
        (tenv, venv, Bool_ty)
      end
  | Unop_expr (unop, expr, pos) ->
    let (_, _, ty) = expr_ty tenv venv expr in begin
    match unop with
    | Not_unop ->
      if ty <> Bool_ty
        then error_unop unop Bool_ty ty pos
        else (tenv, venv, Bool_ty)
    | Neg_unop ->
      if ty <> Number_ty
        then error_unop unop Number_ty ty pos
        else (tenv, venv, Number_ty)
    end
  | Call_expr (name, args, pos) ->
    match SymbolTable.find_opt name venv with
    | Some (Fun_entry (param_types, result_type)) ->
      let get_arg_ty arg =
        let (_, _, ty) = expr_ty tenv venv arg in
        ty
      in
      let args_ty = List.map get_arg_ty args in
      if List.length args_ty <> List.length param_types
        then error_wrong_number_args name pos
        else
          let compare_args (expected, given) =
            if expected = Any_ty then true else expected = given
          in
          if List.for_all compare_args (List.combine param_types args_ty)
            then (tenv, venv, result_type)
            else error_wrong_arg_types name pos
    | Some (Var_entry _) -> error_call_var name pos
    | None -> error_symbol_not_found name pos

and expr_list_ty tenv venv = function
  | [] -> (tenv, venv, Unit_ty)
  | expr :: exprs ->
    let (tenv, venv, _) = expr_ty tenv venv expr in
    expr_list_ty tenv venv exprs

let type_check_fun_decl tenv venv f =
  let find_param_ty field =
    match SymbolTable.find_opt field.field_type tenv with
    | Some ty -> (field.field_name, ty)
    | None -> error_type_not_found field.field_type field.field_pos
  in
  let param_list = List.map find_param_ty f.fundecl_params in
  let load_param venv (name, ty) =
    SymbolTable.add name (Var_entry ty) venv
  in
  let venv = List.fold_left load_param venv param_list in
  let (_, _, body_ty) = expr_list_ty tenv venv f.fundecl_body in
  let result_ty =
    match SymbolTable.find_opt f.fundecl_result tenv with
    | Some ty -> ty
    | None -> error_type_not_found f.fundecl_result f.fundecl_pos
  in
  if body_ty <> result_ty
    then error_type_mismatch result_ty body_ty f.fundecl_pos

let type_check_decl tenv venv = function
  | Fun_decl f -> type_check_fun_decl tenv venv f
  | _ -> raise NotImplemented

let param_types tenv params =
  let param_type field =
    match SymbolTable.find_opt field.field_type tenv with
    | Some ty -> ty
    | None -> error_type_not_found field.field_type field.field_pos
  in
  List.map param_type params

let type_check program =
  let tenv = base_tenv in
  let load_fun_decl venv = function
    | Fun_decl f ->
      let params_tys = param_types tenv f.fundecl_params in
      let result_ty =
        match SymbolTable.find_opt f.fundecl_result tenv with
        | Some ty -> ty
        | None -> error_type_not_found f.fundecl_result f.fundecl_pos
      in
      let entry = Fun_entry (params_tys, result_ty) in
      SymbolTable.add f.fundecl_name entry venv
    | Type_decl _ -> venv
  in
  let venv = List.fold_left load_fun_decl base_venv program in
  List.iter (type_check_decl tenv venv) program
