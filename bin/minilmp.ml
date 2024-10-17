

type integers = 
|Unbound
|MInt of int;;

type variable = string;;
type ops = 
 |Variable of variable
 |Constants of integers
 |Plus of ops * ops
 |Minus of ops * ops
 |Times of ops * ops;;


type boolean = 
|True
|False
|And of boolean * boolean
|Not of boolean
|Minus of ops * ops;;

type command = 
|Skip
|Assign of variable * ops 
|CommandSeq of command * command
|IfThenElse of boolean * command * command
|WhileDo of boolean * command;;


type program = 
|Program of variable * variable * command;;

type 't env = variable -> 't;;
let emptyenv = function x -> Unbound;;
let bind (s: integers env) (x: variable) (v: integers) = 
  function (i: variable) -> if (i = x) then v else (s i)
let my_program = Program("in","out",
CommandSeq(
  Assign("x", Variable("in")),
  CommandSeq(
    Assign("out", Constants(MInt(0))),
    WhileDo(
      Not(
        Minus(
          Variable("x"),
          Constants(MInt(1))
          )
        ),
        CommandSeq(
          Assign("out",Plus(Variable("out"),Variable("x"))),
          Assign("x",Minus(Variable("x"),Constants(MInt(1))))
        )
    )
  )
)
);;

  
let rec op_eval env op =
  let eval_integers = function
  | Unbound -> 0
  | MInt(value) -> value in
  match op with
  |Constants(value) -> eval_integers value
  |Variable(variable) -> eval_integers (env variable)
  |Plus(term1, term2) ->  (op_eval env term1) + (op_eval env term2) 
  |Minus(term1, term2) -> (op_eval env term1) - (op_eval env term2) 
  |Times(term1, term2) -> (op_eval env term1) * (op_eval env term2) ;; 

let rec bool_eval env boolean = 
  match boolean with
  | True -> true
  | False -> false
  | Not(value) -> not (bool_eval env value)
  | And(expr1, expr2) -> (bool_eval env expr1) && (bool_eval env expr2)
  | Minus(op1, op2) -> (op_eval env op1) < (op_eval env op2);;

let rec command_eval env command =
  match command with
  | Skip -> env
  | Assign(variable, op) -> bind env variable (MInt(op_eval env op))
  | CommandSeq(cm1, cm2) -> command_eval (command_eval env cm1) cm2
  | IfThenElse(condition, if_true, if_false) -> if (bool_eval env condition) then command_eval env if_true else command_eval env if_false
  | WhileDo(condition, body) -> if (bool_eval env condition) then command_eval (command_eval env body) (WhileDo(condition, body)) else env;;
let rec eval program input_value = 
  match program with
  | Program(input, output, command) -> (match input_value with
  | MInt(_) -> (command_eval (bind emptyenv input input_value) command) output
  | _ -> failwith "Not a valid program");;

let res = match (eval my_program (MInt(2))) with
 |MInt(value) -> value
 |Unbound -> -1;;
Printf.sprintf "%d" (res);;