open Token;;

val scan_token : In_channel.t -> token

(**/**)
val scan_whitespace : char -> In_channel.t -> char * token option
val scan_digit : char -> In_channel.t -> char * token option
val scan_identifier : char -> In_channel.t -> char * token option


