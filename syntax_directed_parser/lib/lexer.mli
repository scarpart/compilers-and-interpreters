val scan_token : In_channel.t -> Token.t

val next_peek_exn : In_channel.t -> char

(**/**)
val scan_whitespace : char -> In_channel.t -> char * Token.t option
val scan_digit : char -> In_channel.t -> char * Token.t option
val scan_identifier : char -> In_channel.t -> char * Token.t option
val scan_comparison_operator : char -> In_channel.t -> char * Token.t option
val scan_operator : char -> In_channel.t -> char * Token.t option
