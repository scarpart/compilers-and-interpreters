open Ppx_deriving_runtime;;

type t = 
  | Id of string 
  | String of string
  | Int of int
  | Float of float
  | Operator of Operator.operator
  | Symbol of Symbol.symbol
[@@deriving show];;

let token_equal t1 t2 = 
  match (t1, t2) with 
  | Id(s1), Id(s2) -> String.equal s1 s2
  | String(s1), String(s2) -> String.equal s1 s2
  | Int(i1), Int(i2) -> i1 = i2
  | Float(f1), Float(f2) -> Float.equal f1 f2
  | Operator(op1), Operator(op2) -> Operator.equal op1 op2
  | Symbol(s1), Symbol(s2) -> Symbol.equal s1 s2 
  | _ -> false
;;

let token_testable = Alcotest.testable pp token_equal
