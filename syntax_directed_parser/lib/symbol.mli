type symbol = 
  | OpenParen | CloseParen
;;

val pp_symbol : Format.formatter -> symbol -> unit;;
val of_char : char -> symbol;; 
val is_symbol : char -> bool;;
val equal : symbol -> symbol -> bool;;
