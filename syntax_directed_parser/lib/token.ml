type terminal = 
    Id | Digit | String 

type token = {
  terminal: terminal;
  lexeme: string;
} 

let create_token terminal lexeme = { terminal; lexeme; }
