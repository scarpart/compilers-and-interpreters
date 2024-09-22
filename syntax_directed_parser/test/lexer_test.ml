open Alcotest;;
open Syntax_directed_parser.Token;;
open Syntax_directed_parser.Lexer;;
open Core;;
open Core_unix;;

let create_in_channel_of_source s = 
  In_channel.create ~binary:false s 

let test_scan_input () = 
  let input = create_in_channel_of_source "for ( var = 32; var < 400; var = var + 32 )" in 
  let t1 = create_token_lexeme Id "for" in 
  let t2 = scan_token input in 
  Alcotest.(check token_testable) "same token" t1 t2

let test_scan_digit () = 
  let input = create_in_channel_of_source "43243 + 3217893131" in 
  let t1 = create_token_value Digit 43243 in 
  let t2 = scan_token input in 
  Alcotest.(check token_testable) "same token" t1 t2

let () =
  Alcotest.run "Lexer"
    [
      ( "lexer-scanning",
        [
          Alcotest.test_case "Scans reserved keyword" `Quick test_scan_input;
          Alcotest.test_case "Scans digit" `Quick test_scan_digit;
        ] 
      );
    ]
