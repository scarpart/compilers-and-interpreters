type operator = 
  | Lt | Lteq 
  | Gt | Gteq 
  | Eq | Neq
;;

val pp_operator : Format.formatter -> operator -> unit;;
val equal : operator -> operator -> bool;;
