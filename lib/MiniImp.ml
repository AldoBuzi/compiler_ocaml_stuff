type integers = int [@@deriving show];;

type variable = string [@@deriving show];;
type ops = 
 |Variable of variable
 |Constant of integers
 |Plus of ops * ops
 |Minus of ops * ops
 |Times of ops * ops [@@deriving show];;


type boolean = 
|True
|False
|And of boolean * boolean
|Not of boolean
|Smaller of ops * ops [@@deriving show];;

type command = 
|Skip
|Assign of variable * ops 
|CommandSeq of command * command
|IfThenElse of boolean * command * command
|WhileDo of boolean * command [@@deriving show];;


type program = 
|Program of variable * variable * command;;

let my_hash = Hashtbl.create 256;;



let bind (s: ('a, 'b) Hashtbl.t) (x: variable) (v: integers) = Hashtbl.replace s x v; s;;

let find table variable = 
  try
     Hashtbl.find table variable
  with 
   _ -> failwith (Printf.sprintf "Error: Variable %s is not defined" (variable));;

  
let rec eval_ops env op =
  match op with
  |Constant(value) ->  value
  |Variable(variable) -> (find env variable)
  |Plus(term1, term2) ->  (eval_ops env term1) + (eval_ops env term2) 
  |Minus(term1, term2) -> (eval_ops env term1) - (eval_ops env term2) 
  |Times(term1, term2) -> (eval_ops env term1) * (eval_ops env term2) ;; 

let rec eval_bool env boolean = 
  match boolean with
  | True -> true
  | False -> false
  | Not(value) -> not (eval_bool env value)
  | And(expr1, expr2) -> (eval_bool env expr1) && (eval_bool env expr2)
  | Smaller(op1, op2) -> (eval_ops env op1) < (eval_ops env op2);;

let rec eval_command env command =
  match command with
  | Skip -> env
  | Assign(variable, op) -> bind env variable (eval_ops env op)
  | CommandSeq(cm1, cm2) -> eval_command (eval_command env cm1) cm2
  | IfThenElse(condition, if_true, if_false) -> if (eval_bool env condition) then eval_command env if_true else eval_command env if_false
  | WhileDo(condition, body) -> if (eval_bool env condition) then eval_command (eval_command env body) (WhileDo(condition, body)) else env;;

  
let eval program input_value = 
  match program with
  | Program(input, output, command) -> let env = bind my_hash input input_value in find (eval_command env command) output;;