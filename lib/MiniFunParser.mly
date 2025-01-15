(* code to be copied in the scanner module *)
%{
	open MiniFun
%}

(* tokens *)
%token <int> INT
%token <bool> BOOL
%token <string> VARIABLE
%token PLUS MINUS TIMES NOT AND IF THEN ELSE SMALLER GREATER EOF
%token LET IN LETFUN
%token FUN
%token EQUAL L_PAR R_PAR ARROW
%token APPLY
(* start nonterminal *)
%start <term> prg

(* associativity in order of precedence *)
%left IN
%left ELSE ARROW
%right AND
%left NOT
%left SMALLER GREATER  /* lowest precedence */
%left PLUS MINUS  /* lowest precedence */
%left TIMES       /* highest precedence */
%%

(* grammar *)

prg: 
    | t = expr5; EOF               {t}

expr5:
    | LET; t1=VARIABLE; EQUAL; t2 = expr5; IN; t3=expr5 {LetIn(t1,t2,t3)}
    | LETFUN; t1=VARIABLE; t2=VARIABLE; EQUAL; t3 = expr5; IN; t4=expr5 {LetFunIn(t1,t2,t3,t4)}
    (* fun x -> x 1 is right associative, so it returns an high order function *)
    (* fun x -> fun y -> y 1 is equal to (fun x -> (fun y -> (y 1))) *)
    | FUN; t1=VARIABLE; ARROW; t2 = expr5; {Fun(t1,t2)}
    | IF; t1 = expr5; THEN; t2 = expr5; ELSE; t3 = expr5 {IfThenElse(t1,t2,t3)}
    | NOT; t1 = expr5 {Not t1}
    | t1 = expr5; AND; t2 = expr5 { And (t1, t2)}
    | t1 = expr5; PLUS; t2 = expr5  {Plus (t1, t2)}
    | t1 = expr5; MINUS; t2 = expr5  {Minus (t1, t2)}
    | t1 = expr5; TIMES; t2 = expr5 {Times (t1, t2)}
    | t1 = expr5; SMALLER; t2 = expr5  {Smaller (t1, t2)}
    | t1 = expr5; GREATER; t2 = expr5  {Greater (t1, t2)}
    | t = expr2 {t}
(* apply is left associative, this means that f g z y is applied like ((f g) z) y *)
expr2:
    | t1=expr2; t2 = factor { Apply(t1,t2)}
    | t1 = factor {t1}
factor: 
    | t = int {t}
    | i = VARIABLE               {Variable i}
    | i = BOOL {Bool i}
    | L_PAR t = expr5 R_PAR {t}
int:
    | t = INT {Int t}
    |  L_PAR; MINUS; t = INT; R_PAR {Int (-t)}

(* Another way to see the same grammar *)
/* 
prg: 
    | t = expr5; EOF               {t}

expr5:
    | LET; t1=VARIABLE; EQUAL; t2 = expr3; IN; t3=expr5 {LetIn(t1,t2,t3)}
    | LETFUN; t1=VARIABLE; t2=VARIABLE; EQUAL; t3 = expr3; IN; t4=expr5 {LetFunIn(t1,t2,t3,t4)}
    (* fun x -> x 1 is right associative, so it returns an high order function *)
    (* fun x -> fun y -> y 1 is equal to (fun x -> (fun y -> (y 1))) *)
    | FUN; t1=VARIABLE; ARROW; t2 = expr5; {Fun(t1,t2)}
    | IF; t1 = expr5; THEN; t2 = expr5; ELSE; t3 = expr5 {IfThenElse(t1,t2,t3)}
    | t = expr1 {t}

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
    | t = int {t}
    | i = VARIABLE               {Variable i}
    | i = BOOL {Bool i}
    | L_PAR t = expr5 R_PAR {t}
int:
    | t = INT {Int t}
    |  L_PAR; MINUS; t = INT; R_PAR {Int (-t)}
 */