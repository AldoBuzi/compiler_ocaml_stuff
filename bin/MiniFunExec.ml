open Lexing
let () =
  let colnum pos =
    (pos.pos_cnum - pos.pos_bol) - 1 in
  let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c in
  if Array.length Sys.argv != 2 then
    failwith "Argument MiniFun-program is needed";
  let in_file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel in_file in
  let program = (try MiniLang.MiniFunParser.prg MiniLang.MiniFunLexer.read lexbuf 
with MiniLang.MiniFunParser.Error -> raise (Failure ("Parse error at " ^ (pos_string lexbuf.lex_curr_p)))
  ) in
    print_endline (MiniLang.MiniFun.show_evt (MiniLang.MiniFun.eval MiniLang.MiniFun.emptyenv program));
    print_newline()