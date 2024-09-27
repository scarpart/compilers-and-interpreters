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
      ("+", create_token_lexeme Id "+");
      ("-", create_token_lexeme Id "-");
      ("/", create_token_lexeme Id "/");
      ("*", create_token_lexeme Id "*");
      ("=", create_token_lexeme Id "=");
      ("<", create_token_lexeme Id "<");
      (">", create_token_lexeme Id ">");
    ] 
  in 
  Map.of_sequence_exn (module String) seq
;;

let string_table = ref (create_string_table ());; 

(* TODO: change this into something better later *)
let is_char_symbol = function 
  | '(' | ')' | '+' | '-' | '/' 
  | '*' | '=' | '<' | '>' | ';' -> true
  | _ -> false
;;

let next_peek_exn channel = 
  Option.value_exn (In_channel.input_char channel)
;;

let cur_scan_pos chan =
  Option.value_exn (Int64.to_int (In_channel.pos chan))
;;

(*let seek_back ?(n=1) channel = 
*  In_channel.seek channel (Int64.(-) (In_channel.pos channel) (Int64.of_int n))
*;;*)

let seek_back_absolute ?(n=0) channel = 
  In_channel.seek channel (Int64.of_int n) 
;;

let scan_whitespace peek channel : char * token option = 
  let rec scan_whitespace_aux peek is_comment =
    match peek with 
    | _ when is_comment ->
      scan_whitespace_aux (next_peek_exn channel) is_comment
    | x when Char.is_whitespace x || (Char.equal '\n' x) ->
      scan_whitespace_aux (next_peek_exn channel) false
    | x when Char.equal '/' x -> 
      let next = (next_peek_exn channel) in 
      if Char.equal '/' next then
        scan_whitespace_aux (next_peek_exn channel) true
      else (next, None) (* TODO: probably go back one position here *)
    | _ -> (peek, None)
  in 
  scan_whitespace_aux peek false
;;

let char_value_to_int char = 
  Char.to_int char - Char.to_int '0'
;;

let scan_digit peek channel = 
  let rec scan_digit_aux ?(is_digit = false) peek number = 
    if Char.is_digit peek then 
      let next = next_peek_exn channel in 
      scan_digit_aux next (number * 10 + char_value_to_int peek) ~is_digit:true
    else 
    if is_digit then (peek, Some (create_token_value Digit number)) else (peek, None)
  in 
  scan_digit_aux peek 0 ~is_digit:false
;;

let scan_identifier_position peek channel = 
  (* -1 because we're already receiving the peek here 
   * i.e. starting from an already parsed position *)
  let init_pos = Int.of_int64_exn (In_channel.pos channel) - 1 in 
  let rec scan_identifier_pos_aux peek pos = 
    if Char.is_alphanum peek || is_char_symbol peek then
      scan_identifier_pos_aux 
        (Option.value_exn (In_channel.input_char channel)) (pos + 1)
    else (peek, (init_pos, pos))
  in 
  scan_identifier_pos_aux peek init_pos
;;

let lexeme_of_input chan init_pos end_pos = 
  seek_back_absolute chan ~n:init_pos;
  (* Normalize init position to zero otherwise it is not a valid position for the buffer *)
  let rel_init_pos = init_pos - (cur_scan_pos chan) in
  let len = end_pos - init_pos in 
  let buf = Bytes.create_local len in 
  (
    Printf.printf "current channel pos = %d\n%!" (Int.of_int64_exn (In_channel.pos chan));
    Printf.printf "rel_init_pos=%d\ninit_pos=%d\nendpos=%d\nlen=%d\nbuflen=%d\n%!" 
      rel_init_pos init_pos end_pos len (Bytes.length buf);
    In_channel.really_input_exn chan ~pos:rel_init_pos ~buf ~len;
  );
  Bytes.to_string buf  
;;

let scan_identifier peek channel = 
  let (peek, (init_pos, end_pos)) = scan_identifier_position peek channel in 
  let lexeme = lexeme_of_input channel init_pos end_pos in 
  match Map.find !string_table lexeme with 
  | None -> 
    let token = create_token_lexeme Id lexeme in 
    string_table := Map.set !string_table ~key:lexeme ~data:token;
    (peek, Some token)
  | Some _ as some_token -> (peek, some_token)
;;

let rec _or peek list = 
  match list with 
  | [] -> (peek, None)
  | f::fs ->
    match f (peek) with 
    | (next, None) -> _or next fs
    | (next, some_token) -> (next, some_token)
;;

let scan_token channel = 
  let scan_token_helper () = 
    let peek = Option.value_exn (In_channel.input_char channel) in
    let token = _or peek [
        (fun (next) -> scan_whitespace next channel);
        (fun (next) -> scan_digit next channel);
        (fun (next) -> scan_identifier next channel);
      ] 
    in 
    match token with 
    | (next, None) -> create_token_lexeme Unknown (String.of_char next)
    | (_, Some t) -> t
  in
  let result = scan_token_helper () in
  result
;;


