(* code to be copied in the scanner module *)
%{
	open MiniImp
%}

(* tokens *)
%token DEF_MAIN_WITH_IN OUTPUT SEMICOLON AS SKIP EOF
%token ASSIGN IF THEN ELSE WHILE DO L_PAR R_PAR
%token PLUS MINUS TIMES SMALLER NOT AND
%token <string> VARIABLE
%token <int> CONSTANT
%token T_BOOL
%token F_BOOL

(* start nonterminal *)
%start <program> prg

%left AND NOT
%left PLUS MINUS
%left TIMES


%%
(* grammar *)

prg:
    |t = def; EOF {t}
def:
    |DEF_MAIN_WITH_IN; t1 = VARIABLE; OUTPUT; t2 = VARIABLE; AS; body = cmd {Program(t1,t2,body)}
(* left associativity for sequences of commands *)
cmd:
    |t1=cmd; SEMICOLON; t2=expr {CommandSeq(t1,t2)}
    (* semicolon on last instruction is optional *)
    |t1=cmd; SEMICOLON {t1}
    |t1=expr {t1}
expr:
    |t1=VARIABLE; ASSIGN; t2=ops {Assign(t1,t2)}
    |WHILE; t1 = boolean; DO; t2 = expr {WhileDo(t1,t2)}
    (* notice that we can write sequences of commands without using parenthesis within the if_true body, the else_body requires parenthesis for sequences *)
    |IF; t1=boolean; THEN t2=cmd; ELSE; t3=expr {IfThenElse(t1,t2,t3)}
    |L_PAR; t1=cmd; R_PAR {t1}
    |SKIP {Skip}
boolean: 
    |T_BOOL; {True}
    |F_BOOL; {False}
    |t1=boolean; AND; t2=boolean {And(t1,t2)} 
    |NOT; t1=boolean {Not(t1)}
    |t1=ops; SMALLER; t2=ops {Smaller(t1,t2)}

ops:
    |t1=VARIABLE {Variable t1}
    |t1=CONSTANT {Constant (t1)}
    |t1=ops; PLUS; t2=ops; {Plus (t1, t2)}
    |t1=ops; MINUS; t2=ops; {Minus (t1, t2)}
    |t1=ops; TIMES; t2=ops; {Times (t1, t2)}
    |L_PAR; t1=ops; R_PAR {t1}


