type terminal = 
  | Id | Digit | String 
  | Ignore | Unknown

type token = { terminal: terminal; lexeme: string option; value: int option } 

val create_token_lexeme : terminal -> string -> token
val create_token_value : terminal -> int -> token

val token_equal : token -> token -> bool
val pp_token : Format.formatter -> token -> unit

val token_testable : token Alcotest.testable

