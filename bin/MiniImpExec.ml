open MiniLang.MiniImp
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
  let program = (try MiniLang.MiniImpParser.prg MiniLang.MiniImpLexer.read lexbuf 
with MiniLang.MiniImpParser.Error -> raise (Failure ("Parse error at " ^ (pos_string lexbuf.lex_curr_p)))
  ) in
    let x = (match MiniLang.MiniImp.eval program (MInt 5) with
    MInt(value) ->  value) in
    print_endline ( Printf.sprintf "%d" (x));
    print_newline()
(*let deadlock_program = MiniLang.MiniImp.Program(
  "a",
  "b",
  MiniLang.MiniImp.CommandSeq(
    MiniLang.MiniImp.Assign("x",MiniLang.MiniImp.Constants(MiniLang.MiniImp.MInt(1))),
    MiniLang.MiniImp.Assign("b", MiniLang.MiniImp.Plus(MiniLang.MiniImp.Variable("a"),MiniLang.MiniImp.Plus(MiniLang.MiniImp.Variable("x"), MiniLang.MiniImp.Variable("y"))))
  )
);;

let program = Program(
  "a",
  "out",
  CommandSeq(
    Assign("x",Variable "a"),
    CommandSeq(
      Assign("out",Constants(MInt(0))),
      WhileDo(Not(Smaller(Variable("x"),Constants(MInt(1)))), CommandSeq(
        Assign("out",Plus(Variable("out"),Variable("x"))),
        Assign("x",Plus(Variable("x"),Constants(MInt (-1)))
      ))
    )
  )
  )
);;


let res = match (MiniLang.MiniImp.eval program (MInt(2))) with
 |MInt(value) -> value;;

print_endline (Printf.sprintf "%d" (res));;*)
