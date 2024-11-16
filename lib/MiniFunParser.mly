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
%token EQUAL L_PAR R_PAR

(* start nonterminal *)
%start <term> prg

(* associativity in order of precedence *)
%left SMALLER GREATER  /* lowest precedence */
%left PLUS MINUS  /* lowest precedence */
%left TIMES       /* highest precedence */

%%

(* grammar *)
prg: 
    | t = expr5; EOF               {t}

expr5:
    | LET; t1=VARIABLE; EQUAL; t2 = expr3; IN; t3=expr5 {LetIn(t1,t2,t3)}
    | LET; REC; t1=VARIABLE; t2=VARIABLE; EQUAL; t3 = expr3; IN; t4=expr5 {LetFunIn(t1,t2,t3,t4)}
    | t = expr3 {t}
expr3:
    | FUN; t1=VARIABLE; EQUAL; t2 = expr3; {Fun(t1,t2)}
    | IF; t1 = expr3; THEN; t2 = expr3; ELSE; t3 = expr3 {IfThenElse(t1,t2,t3)}
    | t1 = expr1 {t1}

expr1:
    | NOT; t1 = expr1 {Not t1}
    | t1 = factor; AND; t2 = expr1 { And (t1, t2)}
    | t1 = expr {t1}
expr:
    | t1 = expr; PLUS; t2 = expr  {Plus (t1, t2)}
    | t1 = expr; MINUS; t2 = expr  {Minus (t1, t2)}
    | t1 = expr; TIMES; t2 = expr  {Times (t1, t2)}
    | t1 = expr; SMALLER; t2 = expr  {Smaller (t1, t2)}
    | t1 = expr; GREATER; t2 = expr  {Greater (t1, t2)}
    | t = expr2 {t}
expr2:
    | t1=expr2; t2 = factor { Apply(t1,t2)}
    | t1 = factor {t1}
factor: 
    | i = INT                    {Int i}
    | i = VARIABLE               {Variable i}
    | i = BOOL {Bool i}
    | L_PAR t = expr5 R_PAR {t}