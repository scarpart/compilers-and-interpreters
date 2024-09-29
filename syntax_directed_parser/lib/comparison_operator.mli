type comp_op = 
  | Lt | Lteq 
  | Gt | Gteq 
  | Eq | Neq
;;

val pp_comp_op : Format.formatter -> comp_op -> unit;;
val equal : comp_op -> comp_op -> bool;;
