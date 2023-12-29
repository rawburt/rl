type ty =
  | Unit_ty
  | Any_ty
  | Number_ty
  | Bool_ty
  | String_ty
  | Record_ty of string * (string * ty) list
