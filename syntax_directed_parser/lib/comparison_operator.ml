type comp_op = 
  | Lt | Lteq 
  | Gt | Gteq 
  | Eq | Neq
[@@deriving show];;

let equal op1 op2 =
  match op1, op2 with 
  | Lt, Lt | Lteq, Lteq
  | Gt, Gt | Gteq, Gteq
  | Eq, Eq | Neq, Neq -> true
  | _ -> false
;;

