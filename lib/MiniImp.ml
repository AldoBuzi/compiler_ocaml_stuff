type integers = 
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
|Smaller of ops * ops;;

type command = 
|Skip
|Assign of variable * ops 
|CommandSeq of command * command
|IfThenElse of boolean * command * command
|WhileDo of boolean * command;;


type program = 
|Program of variable * variable * command;;

(*type 't env = variable -> 't;;*)
let my_hash = Hashtbl.create 256;;


let string_of_integer integer =
  match integer with
  |MInt(value) -> value;;

(*let emptyenv = function x -> failwith (Printf.sprintf "Error: Variable %s is not defined" (x));;*)
let bind (s: ('a, 'b) Hashtbl.t) (x: variable) (v: integers) = Hashtbl.replace s x v; Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x (string_of_integer y)) s; print_endline "--------";  s;;

let find table variable = 
  try
     Hashtbl.find table variable
  with 
   _ -> failwith (Printf.sprintf "Error: Variable %s is not defined" (variable));;

  
let rec eval_ops env op =
  let eval_integers = function
  | MInt(value) -> value in
  match op with
  |Constants(value) -> eval_integers value
  |Variable(variable) -> eval_integers (find env variable)
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
  | Assign(variable, op) -> bind env variable (MInt(eval_ops env op))
  | CommandSeq(cm1, cm2) -> eval_command (eval_command env cm1) cm2
  | IfThenElse(condition, if_true, if_false) -> if (eval_bool env condition) then eval_command env if_true else eval_command env if_false
  | WhileDo(condition, body) -> if (eval_bool env condition) then eval_command (eval_command env body) (WhileDo(condition, body)) else env;;
let eval program input_value = 
  match program with
  | Program(input, output, command) -> (match input_value with
  | MInt(_) -> let env = bind my_hash input input_value in find (eval_command env command) output
  );;