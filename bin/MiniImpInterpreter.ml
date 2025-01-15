open MiniLang.MiniImp
open Lexing
let () =
  let colnum pos =
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) in
  let pos_string pos=
  let l = string_of_int pos.pos_lnum
  and c = string_of_int (colnum pos) in
  "lines " ^ l ^ ", column " ^ c in
  if Array.length Sys.argv != 2 then
    failwith "Argument MiniImp-program is needed";
  let in_file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel in_file in  
  let program = (try MiniLang.MiniImpParser.prg MiniLang.MiniImpLexer.read lexbuf 
with MiniLang.MiniImpParser.Error -> raise (Failure ("Parse error at " ^ (pos_string lexbuf.Lexing.lex_curr_p)))
  ) in
    let input_v = print_endline "Write your input: "; read_line () in
    print_endline ( Printf.sprintf "%d" (eval program (int_of_string input_v)));
    print_newline()

