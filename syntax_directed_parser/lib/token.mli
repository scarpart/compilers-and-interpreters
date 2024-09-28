type t = 
  | Id of string 
  | String of string
  | Int of int
  | Float of float
  | Operator of Operator.operator
  | Symbol of Symbol.symbol
;;

val token_equal : t -> t -> bool;;
val token_testable : t Alcotest.testable;;
