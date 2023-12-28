open Types

module SymbolTable = Map.Make(String)

type entry =
  | Var_entry of ty
  | Fun_entry of ty list * ty

let base_venv : entry SymbolTable.t =
  SymbolTable.of_list [
    ("output", Fun_entry ([Any_ty], Unit_ty)) ;
  ]

let base_tenv : ty SymbolTable.t =
  SymbolTable.of_list [
    ("unit", Unit_ty) ;
    ("number", Number_ty) ;
    ("bool", Bool_ty) ;
    ("string", String_ty) ;
  ]
