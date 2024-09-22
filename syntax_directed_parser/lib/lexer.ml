open Core;;
open Token;;

let create_string_table () = 
  let seq = Sequence.of_list [
      ("for", create_token_lexeme Id "for");
      (";", create_token_lexeme Id ";");
      ("while", create_token_lexeme Id "while");
      ("var", create_token_lexeme Id "var");
      ("(", create_token_lexeme Id "(");
      (")", create_token_lexeme Id ")");
      ("/", create_token_lexeme Id "/");
      ("*", create_token_lexeme Id "*");
    ] 
  in 
  Map.of_sequence_exn (module String) seq
;;

let string_table = ref (create_string_table ());;

let scan_whitespace channel = 
  let peek = ref (Option.value_exn (In_channel.input_char channel)) in 
  let is_comment = ref false in 
  while (Char.is_whitespace !peek) || !is_comment do 
    peek := Option.value_exn (In_channel.input_char channel);
    if Char.(=) !peek '/' then is_comment := true
    else if Char.(=) !peek '\n' then is_comment := false;
  done;
  Some (create_token_lexeme Ignore "")
;;

let scan_digit channel = 
  let peek = ref (Option.value_exn (In_channel.input_char channel)) in 
  let number = ref 0 in  
  if Char.is_digit !peek then 
    (while Char.is_digit !peek do 
       number := !number * 10 + Char.to_int !peek;
       peek := (Option.value_exn (In_channel.input_char channel))
     done;
     Some (create_token_value Digit !number))
  else None
;;

let scan_identifier_position channel = 
  let peek = ref (Option.value_exn (In_channel.input_char channel)) in 
  let init_pos = In_channel.pos channel in 
  let end_pos = ref init_pos in 
  while Char.is_alphanum !peek do 
    Int64.(end_pos := !end_pos + Int64.of_int 1);
  done;
  (Int.of_int64_exn init_pos, Int.of_int64_exn !end_pos)
;;

let lexeme_of_input channel init_pos end_pos = 
  let _ = In_channel.seek channel (Int64.of_int init_pos) in 
  let len = end_pos - init_pos in 
  let buf = Bytes.create_local len in 
  In_channel.really_input_exn channel ~pos:init_pos ~buf ~len;
  Bytes.to_string buf  
;;

let scan_identifier channel = 
  let (init_pos, end_pos) = scan_identifier_position channel in 
  let lexeme = lexeme_of_input channel init_pos end_pos in 
  match Map.find !string_table lexeme with 
  | None -> 
    let token = create_token_lexeme Id lexeme in 
    string_table := Map.set !string_table ~key:lexeme ~data:token;
    Some token
  | Some _ as some_token -> some_token
;;

let rec _or = function 
  | [] -> None
  | f::fs ->
    match f () with 
    | None -> _or fs
    | Some _ as some_token -> some_token
;;

let scan_token channel = 
  let token = _or [
    (fun () -> scan_whitespace channel);
    (fun () -> scan_digit channel);
    (fun () -> scan_identifier channel);
  ] 
  in 
  match token with 
  | None -> create_token_lexeme Unknown
              (String.of_char
                 (Option.value_exn (In_channel.input_char channel)))
  | Some t -> t
;;



