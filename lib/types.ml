type ty =
  | Unit_ty
  | Any_ty
  | Number_ty
  | Bool_ty
  | String_ty
  | Named_ty of string

let string_of_type = function
  | Unit_ty -> "unit"
  | Any_ty -> "any"
  | Number_ty -> "number"
  | Bool_ty -> "bool"
  | String_ty -> "string"
  | Named_ty name -> name
