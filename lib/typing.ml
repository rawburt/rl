open Ast
open Types

module Ctx : sig
  val tenv_lookup : string -> ty option

  val add_fun : string -> ty list -> ty -> unit
  val add_var : string -> ty -> unit

  val find_fun : string -> (ty list * ty) option
  val find_var : string -> ty option

  val push_scope : unit -> unit
  exception Scope_pop_toplevel
  val pop_scope : unit -> unit

  val debug : string -> unit
  val set_debug : bool -> unit
end = struct
  type entry =
    | Var_entry of ty
    | Fun_entry of ty list * ty
  type venv = (string, entry) Hashtbl.t
  type scoped_venv = {
    parent : scoped_venv option ;
    current : venv ;
  }

  (* mutates when new types are added *)
  let tenv =
    List.to_seq [
      ("unit", Unit_ty) ;
      ("number", Number_ty) ;
      ("bool", Bool_ty) ;
      ("string", String_ty) ;
    ]
    |> Hashtbl.of_seq

  (* mutates on scope change and when entries are added *)
  let venv = ref {
    parent = None ;
    current =
      List.to_seq [
        ("output", Fun_entry ([Any_ty], Unit_ty)) ;
      ]
      |> Hashtbl.of_seq
  }

  let debugging = ref true

  let set_debug d = debugging := d

  let debug str =
    if !debugging then print_endline str

  let tenv_lookup name =
    Hashtbl.find_opt tenv name

  let add_fun name params result =
    Hashtbl.add !venv.current name (Fun_entry (params, result))

  let add_var name ty =
    Hashtbl.add !venv.current name (Var_entry ty)

  let rec venv_lookup v k =
    debug "Ctx.venv_lookup";
    match Hashtbl.find_opt v.current k with
    | Some entry -> Some entry
    | None ->
      match v.parent with
      | Some parent -> venv_lookup parent k
      | None -> None

  let find_var name =
    match venv_lookup !venv name with
    | Some (Var_entry ty) -> Some ty
    | _ -> None

  let find_fun name =
    match venv_lookup !venv name with
    | Some (Fun_entry (args, result)) -> Some (args, result)
    | _ -> None

  let push_scope () =
    debug "Ctx.push_scope";
    let new_venv =
      {
        parent = Some !venv ;
        current = Hashtbl.create 11
      }
    in
    venv := new_venv;

  exception Scope_pop_toplevel

  let pop_scope () =
    debug "Ctx.pop_scope";
    let old_venv = (!venv).parent in
    match old_venv with
    | Some v -> venv := v;
    | None -> raise Scope_pop_toplevel
end

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
  | Call_arg_size of string * int * int
  | Call_arg_types of string * ty list * ty list

exception Type_error of type_error * pos

exception Not_implemented

let rec typecheck_expr = function
  | Bool_expr _ -> Bool_ty
  | Number_expr _ -> Number_ty
  | String_expr _ -> String_ty
  | Var_expr (name, pos) -> begin
    match Ctx.find_var name with
    | Some ty -> ty
    | None -> raise (Type_error (Var_not_found name, pos))
  end
  | Let_expr (name, expr, pos) ->
    typecheck_let_expr name expr pos
  | If_expr (cond_expr, then_exprs, else_exprs, pos) ->
    typecheck_if_expr cond_expr then_exprs else_exprs pos
  | While_expr (cond_expr, body_exprs, pos) ->
    typecheck_while_expr cond_expr body_exprs pos
  | Binop_expr (binop, lhs_expr, rhs_expr, pos) ->
    typecheck_binop_expr binop lhs_expr rhs_expr pos
  | Unop_expr (unop, expr, pos) ->
    typecheck_unop_expr unop expr pos
  | Call_expr (name, args, pos) ->
    typecheck_call_expr name args pos

and typecheck_let_expr name expr pos =
  Ctx.debug "typecheck_let_expr";
  let expr_ty = typecheck_expr expr in
    match Ctx.find_var name with
    | Some ty ->
      (* mutating a var is OK if types are the same *)
      if ty = expr_ty then ty else
        raise (Type_error (Let_mutation (ty, expr_ty), pos))
    | None ->
      Ctx.add_var name expr_ty;
      expr_ty

and typecheck_if_expr cond_expr then_exprs else_exprs pos =
  Ctx.debug "typecheck_if_expr";
  let cond_expr_ty = typecheck_expr cond_expr in
  if cond_expr_ty <> Bool_ty then
    raise (Type_error (If_cond_bool cond_expr_ty, pos))
  else
    Ctx.pop_scope ();
    let then_exprs_type = typecheck_exprs then_exprs in
    Ctx.pop_scope ();
    Ctx.pop_scope ();
    let else_exprs_type = typecheck_exprs else_exprs in
    Ctx.pop_scope ();
    if then_exprs_type = else_exprs_type then then_exprs_type else
      raise (Type_error (If_branches_mismatch (then_exprs_type, else_exprs_type), pos))

and typecheck_while_expr cond_expr body_exprs pos =
  Ctx.debug "typecheck_while_expr";
  let cond_expr_ty = typecheck_expr cond_expr in
  if cond_expr_ty <> Bool_ty then
    raise (Type_error (While_cond_bool cond_expr_ty, pos))
  else
    Ctx.push_scope ();
    let body_exprs_type = typecheck_exprs body_exprs in
    Ctx.pop_scope ();
    body_exprs_type

and typecheck_binop_expr binop lhs_expr rhs_expr pos =
  Ctx.debug "typecheck_binop_expr";
  let lhs_expr_type = typecheck_expr lhs_expr in
  let rhs_expr_type = typecheck_expr rhs_expr in
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

and typecheck_unop_expr unop expr pos =
  Ctx.debug "typecheck_unop_expr";
  let expr_type = typecheck_expr expr in
  match unop with
  | Not_unop ->
    if expr_type = Bool_ty then Bool_ty else
      raise (Type_error (Unop_mismatch (unop, Bool_ty, expr_type), pos))
  | Neg_unop ->
    if expr_type = Number_ty then Number_ty else
      raise (Type_error (Unop_mismatch (unop, Number_ty, expr_type), pos))

and typecheck_call_expr name args pos =
  Ctx.debug "typecheck_call_expr";
  match Ctx.find_fun name with
  | Some (param_types, result_type) ->
    let given_arg_length = List.length args in
    let defined_arg_length = List.length param_types in
    if given_arg_length <> defined_arg_length then
      raise (Type_error (Call_arg_size (name, defined_arg_length, given_arg_length), pos))
    else
      let arg_types = List.map typecheck_expr args in
      let compare_args (defined, passed) =
        if defined = Any_ty then true else defined = passed
      in
      if List.for_all compare_args (List.combine param_types arg_types)
        then result_type
        else raise (Type_error (Call_arg_types (name, param_types, arg_types), pos))
  | None ->
    raise (Type_error (Fun_not_found name, pos))

(* type check expressions in list and return the last expression type *)
and typecheck_exprs = function
  (* empty blocks are of type unit *)
  | [] -> Unit_ty
  | [expr] -> typecheck_expr expr
  | expr :: exprs ->
    let _ = typecheck_expr expr in
    typecheck_exprs exprs

let get_typed_field field =
  Ctx.debug "get_typed_field";
  match Ctx.tenv_lookup field.field_type with
  | Some ty -> (field.field_name, ty)
  | None -> raise (Type_error (Type_not_found field.field_type, field.field_pos))

let fun_decl_result_type f =
  Ctx.debug "fun_decl_result_type";
  match Ctx.tenv_lookup f.fundecl_result with
    | Some ty -> ty
    | None -> raise (Type_error (Type_not_found f.fundecl_result, f.fundecl_pos))

let typecheck_fun_decl f =
  Ctx.debug "typecheck_fun_decl";
  let params_with_type = List.map get_typed_field f.fundecl_params in
  let load_param (name, ty) = Ctx.add_var name ty in
  Ctx.push_scope ();
  List.iter load_param params_with_type;
  let body_type = typecheck_exprs f.fundecl_body in
  let result_type = fun_decl_result_type f in
  Ctx.pop_scope ();
  (* functions defined as unit result type will ignore the body type because there is no return value *)
  if result_type <> Unit_ty && body_type <> result_type then
    raise (Type_error (Neq_fun_body (result_type, body_type), f.fundecl_pos))

let rec typecheck_decls = function
  | [] -> ()
  | Fun_decl f :: decls ->
    typecheck_fun_decl f;
    typecheck_decls decls
  | _ -> raise Not_implemented

let load_fun_decl f =
  Ctx.debug "load_fun_decl";
  let params_with_type = List.map get_typed_field f.fundecl_params in
  let result_type = fun_decl_result_type f in
  Ctx.add_fun f.fundecl_name (List.map snd params_with_type) result_type

let rec load_decls = function
  | [] -> ()
  | Fun_decl f :: decls ->
    load_fun_decl f;
    load_decls decls
  | _ -> raise Not_implemented

let typecheck ?(debug = true) program =
  Ctx.set_debug debug;
  Ctx.debug "typecheck";
  load_decls program;
  typecheck_decls program
