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

type expr =
  | Bool_expr of bool * pos
  | Number_expr of int * pos
  | String_expr of string * pos
  | Var_expr of string * pos
  | Let_expr of string * expr * pos
  | If_expr of expr * expr list * expr list * pos
  | While_expr of expr * expr list * pos
  | Binop_expr of binop * expr * expr * pos
  | Unop_expr of unop * expr * pos
  | Call_expr of string * expr list * pos

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

type typeconst = {
  typeconst_name : string ;
  typeconst_pos : pos ;
}

type typedecl = {
  typedecl_name : string ;
  typedecl_constructors : typeconst list ;
  typedecl_pos : pos ;
}

type decl =
  | Fun_decl of fundecl
  | Type_decl of typedecl
