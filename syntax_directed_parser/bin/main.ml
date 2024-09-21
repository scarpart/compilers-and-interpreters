open Core;;

(* expr -> expr + term /// { print '+' }
 *       | expr - term /// { print '-' }
 *       | term
 *
 * term -> 0 | 1 | ... | 9  /// { print 'term' }
 *
 * Eliminating left-recursion and ambiguous productions 
 * expr  -> term expr'
 * expr' -> + term expr'
 *        | - term expr'
 *        | epsilon 
 * term  -> 0 | 1 | ... | 9
 * *)

let exit_with_newline code =
  Out_channel.printf "\n%!";
  exit code;;

let report err_str = 
  Out_channel.printf "%s%!" err_str;
  exit_with_newline 65;;

let scan source = 
  String.split source ~on:' '

let pop_and_move list = 
  match !list with
  | [] | [_] -> None
  | _::h::t -> begin 
      list := h :: t;
      Some h
    end;;

let put_token token = 
  Out_channel.printf "%s%!" token;
  token

let predictive_parsing tokens = 
  let next current lookahead = 
    if String.equal current lookahead then
      match pop_and_move tokens with
      | Some l -> l 
      | None -> exit_with_newline 64
    else report "Parsing error!"
  in 
  let term lookahead =
    match lookahead with 
    | x when Char.is_digit (Char.of_string x) -> 
      put_token x 
      |> next lookahead
    | _ -> report "Syntax error: expected a digit!"
  in
  let rec rest lookahead = 
    match lookahead with 
    | x when String.equal x "+" || String.equal x "-" -> 
      next x lookahead
      |> term 
      |> put_token
      |> rest 
    | _ -> assert false
  in 
  let expr lookahead = 
    term lookahead |> rest 
  in
  match List.hd !tokens with 
  | Some x -> expr x 
  | None -> report "Syntax error: no tokens were scanned!";
;;

let parse source =
  let tokens = ref (scan source) in 
  predictive_parsing tokens

let () = 
  ignore (parse "4 + 8 - 7 - 9 + 3"); 
;;

