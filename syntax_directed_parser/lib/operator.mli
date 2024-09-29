type operator = 
  | Plus | Minus
  | Division | Multiplication
;;

val pp_operator : Format.formatter -> operator -> unit;;
val of_char : char -> operator option;;
val equal : operator -> operator -> bool;;
