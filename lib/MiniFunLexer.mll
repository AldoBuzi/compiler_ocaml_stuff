(* code to be copied in the scanner module *)
{
open MiniFunParser  (* <-- where we define the tokens *)
exception LexingError of string
}

(* some named RExp *)
let integer = ['0'-'9']['0'-'9']*
let white = [' ' '\t']+ | '\r'
let new_line = '\n'| "\r\n"
let variable = ['a'-'z'](['a'-'z'] | ['0'-'9'])*

(* lexing rules *)
rule read = parse
| "false" {BOOL(bool_of_string (Lexing.lexeme lexbuf))}
| "true" {BOOL(bool_of_string (Lexing.lexeme lexbuf))}
| "fun" {FUN}
| "if" {IF}
| "rec" {REC}
| "then" {THEN}
| "else" {ELSE}
| "not" {NOT}
| "and" {AND}
| "let" {LET}
| "in" {IN}
| new_line {let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with Lexing.pos_lnum = pos.Lexing.pos_lnum + 1; Lexing.pos_bol = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum } ; 
  read lexbuf}
| white {read lexbuf}
| integer {INT(int_of_string (Lexing.lexeme lexbuf))}
| variable {VARIABLE(Lexing.lexeme lexbuf)}
| "=" {EQUAL}
| "->" {ARROW}
| "(" {L_PAR}
| ")" {R_PAR}
| "+" {PLUS}
| "-" {MINUS}
| ">" {GREATER}
| "<" {SMALLER}
| "*" {TIMES}
| eof {EOF}
| _ { raise (LexingError (Lexing.lexeme lexbuf)) }