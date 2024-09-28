open Core;;
open Token;;

let create_string_table () = 
  let seq = Sequence.of_list [
      ("for", Id "for");
      ("while", Id "while");
      ("var", Id "var");
    ] 
  in (* there's probably a better way to make a map than this *)
  Map.of_sequence_exn (module String) seq
;;

let string_table = ref (create_string_table ());; 

let next_peek_exn ch = 
  Option.value_exn (In_channel.input_char ch)
;;

let cur_scan_pos chan =
  Option.value_exn (Int64.to_int (In_channel.pos chan))
;;

(*let seek_back ?(n=1) ch = 
*  In_channel.seek ch (Int64.(-) (In_channel.pos ch) (Int64.of_int n))
*;;*)

let seek_back_absolute ?(n=0) ch = 
  In_channel.seek ch (Int64.of_int n) 
;;

let scan_whitespace peek chan : char * Token.t option = 
  let rec scan_whitespace_aux peek is_comment =
    match peek with 
    | x when Char.equal '\n' x ->
      scan_whitespace_aux (next_peek_exn chan) false
    | x when Char.is_whitespace x || is_comment -> 
      scan_whitespace_aux (next_peek_exn chan) is_comment
    | x when Char.equal '/' x -> 
      let next = (next_peek_exn chan) in 
      if Char.equal '/' next then
        scan_whitespace_aux (next_peek_exn chan) true
      else (next, None) (* TODO: probably go back one position here *)
    | _ -> (peek, None)
  in 
  scan_whitespace_aux peek false
;;

let char_value_to_int char = 
  Char.to_int char - Char.to_int '0'
;;

let scan_digit peek ch = 
  let rec scan_digit_aux ?(is_digit = false) peek number = 
    if Char.is_digit peek then 
      let next = next_peek_exn ch in 
      scan_digit_aux next (number * 10 + char_value_to_int peek) ~is_digit:true
    else 
    if is_digit then (peek, Some (Int (number))) else (peek, None)
  in 
  scan_digit_aux peek 0 ~is_digit:false
;;

let scan_symbol peek _ = 
  if Symbol.is_symbol peek then 
    (peek, Some (Symbol (Symbol.of_char peek)))
  else 
    (peek, None)
;;

let scan_identifier_position peek ch = 
  (* -1 because we're already receiving the peek here 
   * i.e. starting from an already parsed position *)
  let init_pos = Int.of_int64_exn (In_channel.pos ch) - 1 in 
  let rec scan_identifier_pos_aux peek pos = 
    if Char.is_alphanum peek then
      scan_identifier_pos_aux (next_peek_exn ch) (pos + 1)
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
    Printf.printf "current ch pos = %d\n%!" (Int.of_int64_exn (In_channel.pos chan));
    Printf.printf "rel_init_pos=%d\ninit_pos=%d\nendpos=%d\nlen=%d\nbuflen=%d\n%!" 
      rel_init_pos init_pos end_pos len (Bytes.length buf);
    In_channel.really_input_exn chan ~pos:rel_init_pos ~buf ~len;
  );
  Bytes.to_string buf  
;;

let scan_identifier peek ch = 
  let (peek, (init_pos, end_pos)) = scan_identifier_position peek ch in 
  let lexeme = lexeme_of_input ch init_pos end_pos in 
  match Map.find !string_table lexeme with 
  | None -> 
    let token = Id(lexeme) in 
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

let scan_token ch = 
  let scan_token_helper () = 
    let peek = Option.value_exn (In_channel.input_char ch) in
    let token = _or peek [
        (fun (next) -> scan_whitespace next ch);
        (fun (next) -> scan_digit next ch);
        (fun (next) -> scan_symbol next ch);
        (fun (next) -> scan_identifier next ch);
      ] 
    in 
    match token with 
    | (next, None) -> Symbol (Symbol.of_char next)
    | (_, Some t) -> t
  in
  let result = scan_token_helper () in
  result
;;


