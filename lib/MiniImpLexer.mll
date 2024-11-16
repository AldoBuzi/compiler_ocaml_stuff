(* code to be copied in the scanner module *)
{
open MiniImpParser  (* <-- where we define the tokens *)
exception LexingError of string
}

(* named regex *)
let white = [' ' '\t']+ | '\r' | '\n' | "\r\n"
let variable = ['a'-'z'](['a'-'z'] | ['0'-'9'])*
let integer = '-'?['0'-'9']['0'-'9']*


(* lexing rules *)
rule read = parse
| "def main with input in" {DEF_MAIN_WITH_IN}
| ";" {SEMICOLON}
| "output" {OUTPUT}
| "as" {AS}
| ":=" {ASSIGN}
| "true" {T_BOOL}
| "false" {F_BOOL}
| "if" {IF}
| "then" {THEN}
| "else" {ELSE}
| "<" {SMALLER}
| "-" {MINUS}
| "(" {L_PAR}
| ")" {R_PAR}
| "skip" {SKIP}
| white {read lexbuf}
| variable {VARIABLE(Lexing.lexeme lexbuf)}
| integer {CONSTANT(int_of_string (Lexing.lexeme lexbuf))}
| eof {EOF}
| _ { raise (LexingError (Lexing.lexeme lexbuf)) }

