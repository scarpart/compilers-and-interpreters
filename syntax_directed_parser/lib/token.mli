type operator = 
  | Lt | Lteq 
  | Gt | Gteq 
  | Eq | Neq

type t = 
  | Id of string 
  | String of string
  | Int of int
  | Float of float
  | Operator of operator

val token_equal : t -> t -> bool
val pp_token : Format.formatter -> t -> unit

val token_testable : t Alcotest.testable

