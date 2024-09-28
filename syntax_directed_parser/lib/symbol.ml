type symbol = 
  | OpenParen | CloseParen
[@@deriving show];;

let of_char = function 
  | '(' -> OpenParen
  | ')' -> CloseParen
  | _ -> raise (Invalid_argument "The character provided is not a valid symbol.")

let is_symbol = function
  | '(' | ')' -> true
  | _ -> false
;;

let equal s1 s2 =
  match s1, s2 with
  | OpenParen, OpenParen 
  | CloseParen, CloseParen -> true
  | _ -> false
;;
