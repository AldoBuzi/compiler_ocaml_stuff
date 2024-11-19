(* code to be copied in the scanner module *)
%{
	open MiniFun
%}

(* tokens *)
%token <int> INT
%token <bool> BOOL
%token <string> VARIABLE
%token PLUS MINUS TIMES NOT AND IF THEN ELSE SMALLER GREATER EOF
%token LET IN REC
%token FUN
%token EQUAL L_PAR R_PAR ARROW

(* start nonterminal *)
%start <term> prg

(* associativity in order of precedence *)
%left SMALLER GREATER  /* lowest precedence */
%left PLUS MINUS  /* lowest precedence */
%left TIMES       /* highest precedence */

%%

prg: 
    | t = trm; EOF               {t}

trm:
    |LET; t1=VARIABLE; EQUAL; t2 = trm; IN; t3=trm {LetIn(t1,t2,t3)}
    |LET; REC; t1=VARIABLE; t2=VARIABLE; EQUAL; t3 = trm; IN; t4=trm {LetFunIn(t1,t2,t3,t4)}
    |FUN; t1=VARIABLE; ARROW; t2 = trm; {Fun(t1,t2)}
    | IF; t1 = trm; THEN; t2 = trm; ELSE; t3 = trm {IfThenElse(t1,t2,t3)}
    | NOT; t1 = trm {Not t1}
    | t1 = trm; AND; t2 = trm { And (t1, t2)}
    | i = INT                    {Int i}
    | i = VARIABLE               {Variable i}
    | i = BOOL {Bool i}
    | L_PAR t = trm R_PAR {t}

(* grammar *)
(*
prg: 
    | t = expr5; EOF               {t}

expr5:
    | LET; t1=VARIABLE; EQUAL; t2 = expr3; IN; t3=expr5 {LetIn(t1,t2,t3)}
    | LET; REC; t1=VARIABLE; t2=VARIABLE; EQUAL; t3 = expr3; IN; t4=expr5 {LetFunIn(t1,t2,t3,t4)}
    | t = expr3 {t}
expr3:
    (* fun x -> x 1 is right associative, so it returns an high order function *)
    (* fun x -> fun y -> y 1 is equal to (fun x -> (fun y -> (y 1))) *)
    | FUN; t1=VARIABLE; ARROW; t2 = expr3; {Fun(t1,t2)}
    | IF; t1 = expr3; THEN; t2 = expr3; ELSE; t3 = expr3 {IfThenElse(t1,t2,t3)}
    | t1 = expr1 {t1}

expr1:
    | NOT; t1 = expr1 {Not t1}
    (* right associative like ocaml, test not false || false && false -> ocaml returns true  *)
    | t1 = factor; AND; t2 = expr1 { And (t1, t2)}
    | t1 = expr {t1}
expr:
    | t1 = expr; PLUS; t2 = expr  {Plus (t1, t2)}
    | t1 = expr; MINUS; t2 = expr  {Minus (t1, t2)}
    | t1 = expr; TIMES; t2 = expr  {Times (t1, t2)}
    | t1 = expr; SMALLER; t2 = expr  {Smaller (t1, t2)}
    | t1 = expr; GREATER; t2 = expr  {Greater (t1, t2)}
    | t = expr2 {t}
(* apply is left associative, this means that f g z y is applied like ((f g) z) y *)
expr2:
    | t1=expr2; t2 = factor { Apply(t1,t2)}
    | t1 = factor {t1}
factor: 
    | i = INT                    {Int i}
    | i = VARIABLE               {Variable i}
    | i = BOOL {Bool i}
    | L_PAR t = expr5 R_PAR {t}
*)