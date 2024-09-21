type terminal = 
    Id | Digit | String 

type token = { terminal: terminal; lexeme: string; } 

val create_token : terminal -> string -> token


