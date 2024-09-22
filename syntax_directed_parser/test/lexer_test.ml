open Syntax_directed_parser.Token;;
open Syntax_directed_parser.Lexer;;
open Core;;

let create_in_channel_of_source s = 
  let cwd = Core_unix.getcwd () in 
  In_channel.create ~binary:false (cwd ^ "/testfiles/" ^ s)

let test_scan_input () = 
  let input = create_in_channel_of_source "forloop" in 
  let t1 = create_token_lexeme Id "for" in 
  let t2 = scan_token input in 
  Alcotest.(check token_testable) "same token" t1 t2

let test_scan_digit () = 
  let input = create_in_channel_of_source "digits" in 
  let t1 = create_token_value Digit 43243 in 
  let t2 = scan_token input in 
  Alcotest.(check token_testable) "same token" t1 t2

let () =
  Printf.printf "this is a test\n%!";
  Alcotest.run "Lexer"
    [
      ( "lexer-scanning",
        [
          Alcotest.test_case "Scans reserved keyword" `Quick test_scan_input;
          Alcotest.test_case "Scans digit" `Quick test_scan_digit;
        ] 
      );
    ]
