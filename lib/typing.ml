open Ast
open Types

type entry =
  | Var_entry of ty
  | Fun_entry of ty list * ty
type venv = (string, entry) Hashtbl.t
type scoped_venv = {
  parent : scoped_venv option ;
  current : venv ;
}
type tenv = (string, ty) Hashtbl.t

let base_tenv =
  List.to_seq [
    ("unit", Unit_ty) ;
    ("number", Number_ty) ;
    ("bool", Bool_ty) ;
    ("string", String_ty) ;
  ]
  |> Hashtbl.of_seq

let base_venv =
  List.to_seq [
    ("output", Fun_entry ([Any_ty], Unit_ty)) ;
  ]
  |> Hashtbl.of_seq

type ctx = {
  tenv : tenv ;
  mutable venv : scoped_venv ;
  debug : bool
}

let debug ctx str =
  if ctx.debug then print_endline str

let tenv_lookup ctx name =
  Hashtbl.find_opt ctx.tenv name

let add_fun ctx name params result =
  Hashtbl.add ctx.venv.current name (Fun_entry (params, result))

let add_var ctx name ty =
  Hashtbl.add ctx.venv.current name (Var_entry ty)

let add_record_type ctx name field_types =
  Hashtbl.add ctx.tenv name (Record_ty (name, field_types))

let rec venv_lookup ctx v k =
  debug ctx "venv_lookup";
  match Hashtbl.find_opt v.current k with
  | Some entry -> Some entry
  | None ->
    match v.parent with
    | Some parent -> venv_lookup ctx parent k
    | None -> None

let find_var ctx name =
  match venv_lookup ctx ctx.venv name with
  | Some (Var_entry ty) -> Some ty
  | _ -> None

let find_fun ctx name =
  match venv_lookup ctx ctx.venv name with
  | Some (Fun_entry (args, result)) -> Some (args, result)
  | _ -> None

let push_scope ctx =
  debug ctx "push_scope";
  let new_venv =
    {
      parent = Some ctx.venv ;
      current = Hashtbl.create 11
    }
  in
  ctx.venv <- new_venv;

exception Scope_pop_toplevel

let pop_scope ctx =
  debug ctx "pop_scope";
  let old_venv = ctx.venv.parent in
  match old_venv with
  | Some v -> ctx.venv <- v;
  | None -> raise Scope_pop_toplevel

(* pairs are in the form of (expected, got) *)
type type_error =
  | Type_not_found of string
  | Var_not_found of string
  | Fun_not_found of string
  | Neq_fun_body of ty * ty
  | Let_mutation of ty * ty
  | If_cond_bool of ty
  | If_branches_mismatch of ty * ty
  | While_cond_bool of ty
  | Binop_expr_type of binop * ty * ty
  | Binop_expr_match of binop * ty * ty
  | Unop_mismatch of unop * ty * ty
  | Call_record_field of var
  | Call_arg_size of string * int * int
  | Call_arg_types of string * ty list * ty list
  | Record_field_mismatch of string * (string * ty) list * (string * ty) list
  | Record_expected of ty
  | Record_field_not_found of string * (string * ty) list * string

exception Type_error of type_error * pos

let rec typecheck_expr ctx = function
  | Bool_expr _ -> Bool_ty
  | Number_expr _ -> Number_ty
  | String_expr _ -> String_ty
  | Var_expr (v, _) -> typecheck_var ctx v
  | Let_expr (var, expr, pos) ->
    typecheck_let_expr ctx var expr pos
  | If_expr (cond_expr, then_exprs, else_exprs, pos) ->
    typecheck_if_expr ctx cond_expr then_exprs else_exprs pos
  | While_expr (cond_expr, body_exprs, pos) ->
    typecheck_while_expr ctx cond_expr body_exprs pos
  | Binop_expr (binop, lhs_expr, rhs_expr, pos) ->
    typecheck_binop_expr ctx binop lhs_expr rhs_expr pos
  | Unop_expr (unop, expr, pos) ->
    typecheck_unop_expr ctx unop expr pos
  | Call_expr (var, args, pos) ->
    typecheck_call_expr ctx var args pos
  | Record_expr (name, args, pos) ->
    typecheck_record_expr ctx name args pos

and typecheck_var ctx = function
 | Simple_var (name, pos) -> begin
    match find_var ctx name with
    | Some ty -> ty
    | None -> raise (Type_error (Var_not_found name, pos))
  end
  | Field_var (parent, name, pos) ->
    match typecheck_var ctx parent with
    | Record_ty (record_name, record_fields) ->
      let compare_field (n, _) = n = name in begin
      match List.find_opt compare_field record_fields with
      | Some (_, field_type) -> field_type
      | None -> raise (Type_error (Record_field_not_found (record_name, record_fields, name), pos))
    end
    | t -> raise (Type_error (Record_expected t, pos))

and typecheck_let_expr ctx var expr pos =
  debug ctx "typecheck_let_expr";
  let expr_ty = typecheck_expr ctx expr in
  match var with
  | Simple_var (var_name, _) -> begin
    match find_var ctx var_name with
    | Some var_ty ->
      (* mutating a var is OK if types are the same *)
      if var_ty = expr_ty then var_ty else
        raise (Type_error (Let_mutation (var_ty, expr_ty), pos))
    | None ->
      add_var ctx var_name expr_ty;
      expr_ty
    end
  | Field_var _ ->
    let var_ty = typecheck_var ctx var in
    if var_ty = expr_ty then var_ty else
      raise (Type_error (Let_mutation (var_ty, expr_ty), pos))

and typecheck_if_expr ctx cond_expr then_exprs else_exprs pos =
  debug ctx "typecheck_if_expr";
  let cond_expr_ty = typecheck_expr ctx cond_expr in
  if cond_expr_ty <> Bool_ty then
    raise (Type_error (If_cond_bool cond_expr_ty, pos))
  else
    push_scope ctx;
    let then_exprs_type = typecheck_exprs ctx then_exprs in
    pop_scope ctx;
    push_scope ctx;
    let else_exprs_type = typecheck_exprs ctx else_exprs in
    pop_scope ctx;
    if then_exprs_type = else_exprs_type then then_exprs_type else
      raise (Type_error (If_branches_mismatch (then_exprs_type, else_exprs_type), pos))

and typecheck_while_expr ctx cond_expr body_exprs pos =
  debug ctx "typecheck_while_expr";
  let cond_expr_ty = typecheck_expr ctx cond_expr in
  if cond_expr_ty <> Bool_ty then
    raise (Type_error (While_cond_bool cond_expr_ty, pos))
  else
    push_scope ctx;
    let body_exprs_type = typecheck_exprs ctx body_exprs in
    pop_scope ctx;
    body_exprs_type

and typecheck_binop_expr ctx binop lhs_expr rhs_expr pos =
  debug ctx "typecheck_binop_expr";
  let lhs_expr_type = typecheck_expr ctx lhs_expr in
  let rhs_expr_type = typecheck_expr ctx rhs_expr in
  match binop with
  (* (num * num) -> num *)
  | Plus_binop | Minus_binop
  | Times_binop | Divide_binop ->
    if lhs_expr_type <> Number_ty then
      raise (Type_error (Binop_expr_type (binop, Number_ty, lhs_expr_type), pos))
    else
      if lhs_expr_type = rhs_expr_type then Number_ty else
        raise (Type_error (Binop_expr_match (binop, lhs_expr_type, rhs_expr_type), pos))
  (* (num * num) -> bool *)
  | Lt_binop | Le_binop | Gt_binop | Ge_binop ->
    if lhs_expr_type <> Number_ty then
      raise (Type_error (Binop_expr_type (binop, Number_ty, lhs_expr_type), pos))
    else
      if lhs_expr_type = rhs_expr_type then Bool_ty else
        raise (Type_error (Binop_expr_match (binop, lhs_expr_type, rhs_expr_type), pos))
  (* (any * any) -> bool *)
  | Eq_binop | Neq_binop ->
    if lhs_expr_type = rhs_expr_type then Bool_ty else
      raise (Type_error (Binop_expr_match (binop, lhs_expr_type, rhs_expr_type), pos))

and typecheck_unop_expr ctx unop expr pos =
  debug ctx "typecheck_unop_expr";
  let expr_type = typecheck_expr ctx expr in
  match unop with
  | Not_unop ->
    if expr_type = Bool_ty then Bool_ty else
      raise (Type_error (Unop_mismatch (unop, Bool_ty, expr_type), pos))
  | Neg_unop ->
    if expr_type = Number_ty then Number_ty else
      raise (Type_error (Unop_mismatch (unop, Number_ty, expr_type), pos))

and typecheck_call_expr ctx var args pos =
  debug ctx "typecheck_call_expr";
  match var with
  | Field_var _ -> raise (Type_error (Call_record_field var, pos))
  | Simple_var (name, _) ->
    match find_fun ctx name with
    | Some (param_types, result_type) ->
      let given_arg_length = List.length args in
      let defined_arg_length = List.length param_types in
      if given_arg_length <> defined_arg_length then
        raise (Type_error (Call_arg_size (name, defined_arg_length, given_arg_length), pos))
      else
        let arg_types = List.map (typecheck_expr ctx) args in
        let compare_args (defined, passed) =
          if defined = Any_ty then true else defined = passed
        in
        if List.for_all compare_args (List.combine param_types arg_types)
          then result_type
          else raise (Type_error (Call_arg_types (name, param_types, arg_types), pos))
    | None ->
      raise (Type_error (Fun_not_found name, pos))

and typecheck_record_expr ctx name args pos =
  let get_field_type (name, expr) = (name, typecheck_expr ctx expr) in
  let fields_with_types = List.map get_field_type args in
  match tenv_lookup ctx name with
  | Some (Record_ty (name, fields)) ->
    let sorted_fields = List.sort compare fields in
    let sorted_args = List.sort compare fields_with_types in
    if List.equal (=) sorted_fields sorted_args
      then Record_ty (name, fields)
      else raise (Type_error (Record_field_mismatch (name, sorted_fields, sorted_args), pos))
  | _ -> raise (Type_error (Type_not_found name, pos))

(* type check expressions in list and return the last expression type *)
and typecheck_exprs ctx = function
  (* empty blocks are of type unit *)
  | [] -> Unit_ty
  | [expr] -> typecheck_expr ctx expr
  | expr :: exprs ->
    let _ = typecheck_expr ctx expr in
    typecheck_exprs ctx exprs

let get_typed_field ctx field =
  debug ctx "get_typed_field";
  match tenv_lookup ctx field.field_type with
  | Some ty -> (field.field_name, ty)
  | None -> raise (Type_error (Type_not_found field.field_type, field.field_pos))

let fun_decl_result_type ctx f =
  debug ctx "fun_decl_result_type";
  match tenv_lookup ctx f.fundecl_result with
    | Some ty -> ty
    | None -> raise (Type_error (Type_not_found f.fundecl_result, f.fundecl_pos))

let typecheck_fun_decl ctx f =
  debug ctx "typecheck_fun_decl";
  let params_with_type = List.map (get_typed_field ctx) f.fundecl_params in
  let load_param (name, ty) = add_var ctx name ty in
  push_scope ctx;
  List.iter load_param params_with_type;
  let body_type = typecheck_exprs ctx f.fundecl_body in
  let result_type = fun_decl_result_type ctx f in
  pop_scope ctx;
  (* functions defined as unit result type will ignore the body type because there is no return value *)
  if result_type <> Unit_ty && body_type <> result_type then
    raise (Type_error (Neq_fun_body (result_type, body_type), f.fundecl_pos))

let rec typecheck_decls ctx = function
  | [] -> ()
  | Fun_decl f :: decls ->
    typecheck_fun_decl ctx f;
    typecheck_decls ctx decls
  | Record_decl _ :: decls ->
    (* nothing to do *)
    typecheck_decls ctx decls

let load_fun_decl ctx f =
  debug ctx "load_fun_decl";
  let params_with_type = List.map (get_typed_field ctx) f.fundecl_params in
  let result_type = fun_decl_result_type ctx f in
  add_fun ctx f.fundecl_name (List.map snd params_with_type) result_type

let load_record_decl ctx r =
  debug ctx "load_record_decl";
  (* TODO: order independent record definitions *)
  let fields_with_types = List.map (get_typed_field ctx) r.recdecl_params in
  add_record_type ctx r.recdecl_name fields_with_types

let rec load_decls ctx = function
  | [] -> ()
  | Fun_decl f :: decls ->
    load_fun_decl ctx f;
    load_decls ctx decls
  | Record_decl r :: decls ->
    load_record_decl ctx r;
    load_decls ctx decls

let typecheck ?(debug = true) program =
  let ctx =
    {
      tenv = base_tenv ;
      venv = {
        parent = None ;
        current = base_venv ;
      } ;
      debug ;
    }
  in
  load_decls ctx program;
  typecheck_decls ctx program
