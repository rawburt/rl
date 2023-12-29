type pos = Lexing.position

type unop =
  | Not_unop
  | Neg_unop

type binop =
  | Plus_binop
  | Minus_binop
  | Times_binop
  | Divide_binop
  | Eq_binop
  | Neq_binop
  | Lt_binop
  | Le_binop
  | Gt_binop
  | Ge_binop

type var =
  | Simple_var of string * pos
  | Field_var of var * string * pos

type expr =
  | Bool_expr of bool * pos
  | Number_expr of int * pos
  | String_expr of string * pos
  | Var_expr of var * pos
  | Let_expr of string * expr * pos
  | If_expr of expr * expr list * expr list * pos
  | While_expr of expr * expr list * pos
  | Binop_expr of binop * expr * expr * pos
  | Unop_expr of unop * expr * pos
  | Call_expr of string * expr list * pos
  | Record_expr of string * (string * expr) list * pos

type field = {
  field_name : string ;
  field_type : string ;
  field_pos : pos ;
}

type fundecl = {
  fundecl_name : string ;
  fundecl_params : field list ;
  fundecl_result : string ;
  fundecl_body : expr list ;
  fundecl_pos : pos ;
}

type recdecl = {
  recdecl_name : string ;
  recdecl_params : field list ;
  recdecl_pos : pos ;
}

type decl =
  | Fun_decl of fundecl
  | Record_decl of recdecl
