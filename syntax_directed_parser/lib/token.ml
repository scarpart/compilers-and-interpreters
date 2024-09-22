open Ppx_deriving_runtime;;

type terminal = 
  | Id | Digit | String 
  | Ignore | Unknown 
[@@deriving show];;

type token = {
  terminal: terminal;
  lexeme: string option;
  value: int option;
} 
[@@deriving show];;

let create_token_lexeme terminal lexeme = 
  { terminal; lexeme = Some lexeme; value = None; }
let create_token_value terminal value =
  { terminal; lexeme = None; value = Some value; }

let token_equal t1 t2 = 
  t1.terminal = t2.terminal &&
  t1.lexeme = t2.lexeme && 
  t1.value = t2.value 
;;

let token_testable = Alcotest.testable pp_token token_equal

