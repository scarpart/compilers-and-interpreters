open Syntax_directed_parser.Token;;
open Syntax_directed_parser.Lexer;;
open Core;;

let create_in_channel_of_source s = 
  let cwd = Core_unix.getcwd () in 
  In_channel.create ~binary:false (cwd ^ "/testfiles/" ^ s)

let test_scan_whitespace () = 
  let input = create_in_channel_of_source "forloop" in 
  let peek1 = Option.value_exn (In_channel.input_char input) in 
  let (next, opt) = scan_whitespace peek1 input in 
  Alcotest.(check char) "same char" next 'f';
  Alcotest.(check bool) "same option" true (Option.is_none opt)
;;

let test_scan_digit () = 
  let input = create_in_channel_of_source "digits" in 
  let peek1 = Option.value_exn (In_channel.input_char input) in 
  let (next, opt) = scan_digit peek1 input in 
  Alcotest.(check char) "same char" next ' ';
  Alcotest.(check bool) "is some" true (Option.is_some opt);
  let number = (Option.value_exn (Option.value_exn opt).value) in
  (Printf.printf "Got the following number from the token = %d\n%!" number;
  Alcotest.(check bool) "is correct number" true (43243 = number))
;; 

let test_scan_identifier () = 
  let input = create_in_channel_of_source "forloop" in 
  let peek1 = Option.value_exn (In_channel.input_char input) in 
  let (next, opt) = scan_identifier peek1 input in 
  begin
    Alcotest.(check token_testable) "same identifier (for)" 
      { terminal=Id; lexeme=Some "for"; value=None } (Option.value_exn opt);
    Alcotest.(check char) "same next character (whitespace)" ' ' next
  end
;;

let test_scan_input_with_comments () = 
  let input = create_in_channel_of_source "forloop_comment" in 
  let t1_l = create_token_lexeme Id "for" in 
  let t1_r = scan_token input in 
  Alcotest.(check token_testable) "same token (identifier 'for')" t1_l t1_r;
  let t2_l = create_token_lexeme Id "(" in 
  let t2_r = scan_token input in 
  Alcotest.(check token_testable) "same token (identifier '(')" t2_l t2_r
;;

let () =
  Alcotest.run "Lexer"
    [
      ( "lexer-scanning",
        [
          Alcotest.test_case "Scans reserved keyword" `Quick test_scan_input_with_comments;
        ] 
      ); 
      ( "helper-functions", 
        [
          Alcotest.test_case "Scans whitespace" `Quick test_scan_whitespace;
          Alcotest.test_case "Scans digit" `Quick test_scan_digit; 
          Alcotest.test_case "Scans identifier" `Quick test_scan_identifier;
        ]
      );
    ]
