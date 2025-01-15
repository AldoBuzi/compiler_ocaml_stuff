(* code to be copied in the scanner module *)
{
open MiniImpParser  (* <-- where we define the tokens *)
exception LexingError of string
}

(* named regex *)
let white = [' ' '\t']+ | '\r'
let new_line = '\n'| "\r\n"
let variable = ['a'-'z'](['a'-'z'] | ['0'-'9'])*
let integer = ['0'-'9']['0'-'9']*

(* lexing rules *)
rule read = parse
| "def main with input" {DEF_MAIN_WITH_IN}
| ";" {SEMICOLON}
| "output" {OUTPUT}
| "as" {AS}
| ":=" {ASSIGN}
| "true" {T_BOOL}
| "false" {F_BOOL}
| "while" {WHILE}
| "do" {DO}
| "if" {IF}
| "then" {THEN}
| "else" {ELSE}
| "<" {SMALLER}
| "-" {MINUS}
| "+" {PLUS}
| "*" {TIMES}
| "(" {L_PAR}
| ")" {R_PAR}
| "skip" {SKIP}
| "and" {AND}
| new_line {let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with Lexing.pos_lnum = pos.Lexing.pos_lnum + 1; Lexing.pos_bol = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum } ; 
  read lexbuf}
| white {read lexbuf}
| variable {VARIABLE(Lexing.lexeme lexbuf)}
| integer {CONSTANT(int_of_string (Lexing.lexeme lexbuf))}
| eof {EOF}
| _ { raise (LexingError (Lexing.lexeme lexbuf)) }

