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

let test_scan_int () = 
  let input = create_in_channel_of_source "digits" in 
  let peek1 = Option.value_exn (In_channel.input_char input) in 
  let (next, opt) = scan_digit peek1 input in 
  Alcotest.(check char) "same char" next ' ';
  Alcotest.(check bool) "is some" true (Option.is_some opt);
  let n_opt = match (Option.value_exn opt) with 
    | Int(x) -> Some x
    | _ -> None
  in
  if Option.is_some n_opt then 
    let n = Option.value_exn n_opt in 
    (Printf.printf "Got the following number from the token = %d\n%!" n;
     Alcotest.(check bool) "is correct number" true (43243 = n))
  else 
    Alcotest.(check bool) "is correct number type" true false
;; 

let test_scan_identifier () = 
  let input = create_in_channel_of_source "forloop" in 
  let peek1 = Option.value_exn (In_channel.input_char input) in 
  let (next, opt) = scan_identifier peek1 input in 
  begin
    Alcotest.(check token_testable) "same identifier (for)" 
      (Id "for") (Option.value_exn opt);
    Alcotest.(check char) "same next character (whitespace)" ' ' next
  end
;;

let test_scan_input () =
  let input = create_in_channel_of_source "dragon_book_ex1" in 
  let t1_1 = Symbol OpenParen in
  let t2_1 = scan_token input in 
  Alcotest.(check token_testable) "same token (parenthesis symbol)" t1_1 t2_1;
  let t1_2 = Int 32 in 
  let t2_2 = scan_token input in 
  Alcotest.(check token_testable) "same token (integer)" t1_2 t2_2
;;

let test_scan_input_with_comments () = 
  let input = create_in_channel_of_source "dragon_book_ex1_comment" in 
  let t1_l = Symbol OpenParen in 
  let t1_r = scan_token input in 
  Alcotest.(check token_testable) "same token (symbol '(')" t1_l t1_r;
  let t2_l = Int 32 in 
  let t2_r = scan_token input in 
  Alcotest.(check token_testable) "same token (int '32')" t2_l t2_r
;;

(*
let test_scan_input_with_comments () = 
  let input = create_in_channel_of_source "forloop_comment" in 
  let t1_l = Id "for" in 
  let t1_r = scan_token input in 
  Alcotest.(check token_testable) "same token (id 'for')" t1_l t1_r;
  let t2_l = Symbol OpenParen in 
  let t2_r = scan_token input in 
  Alcotest.(check token_testable) "same token (symbol '(')" t2_l t2_r
;;
*)

let () =
  Alcotest.run "Lexer"
    [
      ( "lexer-scanning",
        [
          Alcotest.test_case "Scans reserved keyword" `Quick test_scan_input_with_comments;
          Alcotest.test_case "Scans simple input" `Quick test_scan_input
        ] 
      ); 
      ( "helper-functions", 
        [
          Alcotest.test_case "Scans whitespace" `Quick test_scan_whitespace;
          Alcotest.test_case "Scans digit" `Quick test_scan_int; 
          Alcotest.test_case "Scans identifier" `Quick test_scan_identifier;
        ]
      );
    ]
