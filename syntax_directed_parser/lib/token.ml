open Ppx_deriving_runtime;;

type operator = 
  | Lt | Lteq 
  | Gt | Gteq 
  | Eq | Neq
[@@deriving show];;

type token = 
  | Id of string 
  | String of string
  | Int of int
  | Float of float
  | Operator of operator
[@@deriving show];;

let operator_equal op1 op2 =
  match op1, op2 with 
  | Lt, Lt | Lteq, Lteq
  | Gt, Gt | Gteq, Gteq
  | Eq, Eq | Neq, Neq -> true
  | _ -> false
;;

let token_equal t1 t2 = 
  match (t1, t2) with 
  | Id(s1), Id(s2) -> String.equal s1 s2
  | String(s1), String(s2) -> String.equal s1 s2
  | Int(i1), Int(i2) -> i1 = i2
  | Float(f1), Float(f2) -> Float.equal f1 f2
  | Operator(op1), Operator(op2) -> operator_equal op1 op2
  | _ -> false
;;

let token_testable = Alcotest.testable pp_token token_equal

