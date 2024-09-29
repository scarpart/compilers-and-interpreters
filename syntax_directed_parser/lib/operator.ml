type operator = 
  | Plus | Minus
  | Division | Multiplication
[@@deriving show];;

let of_char = function 
  | '+' -> Some Plus
  | '-' -> Some Minus
  | '/' -> Some Division
  | '*' -> Some Multiplication
  | _ -> None
;;

let equal op1 op2 =
  match op1, op2 with 
  | Plus, Plus
  | Minus, Minus 
  | Division, Division
  | Multiplication, Multiplication -> true
  | _ -> false
;;
