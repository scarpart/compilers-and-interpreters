type terminal = 
    Id | Digit | String 

type token = {
  terminal: terminal;
  lexeme: string option;
  value: int option;
} 

let create_token_lexeme terminal lexeme = 
  { terminal; lexeme = Some lexeme; value = None; }
let create_token_value terminal value =
  { terminal; lexeme = None; value = Some value; }
