
type variable = string [@@deriving show];;

type term =
  |Int of int
  |Bool of bool
  |Variable of variable
  |Fun of variable * term
  |Apply of term * term
  |Plus of term * term
  |Substraction of term * term
  |Times of term * term
  |Minus of term * term
  |Not of term
  |And of term * term
  |IfThenElse of term * term * term
  |LetIn of variable * term * term
  |LetFunIn of variable * variable * term * term [@@deriving show];;


type 't env = variable -> 't [@@deriving show];;

type evt =
|EInt of int
|EBool of bool
|Closure of variable * term * evt env
|RecClosure of variable * variable * term * evt env [@@deriving show];;

let evt_to_string = function 
  |EInt(value) -> Printf.sprintf "%d" value
  |EBool(value) -> Printf.sprintf "%b" value
  |_ -> "Closure not printable";;

let emptyenv = function x -> failwith (Printf.sprintf "Variable %s not found in env" x);;
let bind (s: evt env) (x: variable) (v: evt) = 
  function (i: variable) -> if (i = x) then v else (s i)



let rec eval env program = 
  match program with
  | Int(value) -> EInt(value)
  | Bool(value) -> EBool(value)
  | Variable(value) -> env value
  | Fun(arg, body) -> Closure((arg, body, env))
  | IfThenElse(condition, if_true, if_false) -> (match (eval env condition) with
    | EBool(true) -> eval env if_true
    | EBool(false) -> eval env if_false
    | _ -> failwith "Condition does not return a boolean")
  | LetIn(variable, body, in_context) -> eval (bind env variable (eval env body)) in_context
  | Apply(t1, t2) -> (match eval env t1 with
    | Closure(arg,body,_env) ->  eval (bind _env arg (eval env t2)) body
    | RecClosure(fname, arg, body, _env) ->  eval (bind (bind _env fname (eval env t1)) arg (eval env t2)) body 
    | _ -> failwith "You are applying to a term that is not a function")
  | LetFunIn(fname, arg, body, in_context) -> eval (bind env fname (RecClosure(fname,arg,body, env))) in_context
  | Plus(op1, op2) -> EInt( match ((eval env op1), (eval env op2)) with
    | (EInt(val1), EInt(val2)) -> val1+val2
    | _ -> failwith "Term provided is not a integer")
  | Minus(op1,op2) -> EBool( match ((eval env op1), (eval env op2)) with
    | (EInt(val1), EInt(val2)) -> val1 < val2
    | _ -> failwith "Term provided is not a boolean")
  | Substraction(op1,op2) -> EInt( match ((eval env op1), (eval env op2)) with
    | (EInt(val1), EInt(val2)) -> val1-val2
    | _ -> failwith "Term provided is not a integer")
  | Not (op) -> EBool( match (eval env op) with
    | EBool(val1) -> not val1
    | _ -> failwith "Term provided is not a boolean")
  | Times(op1,op2) -> EInt( match ((eval env op1), (eval env op2)) with
    | (EInt(val1), EInt(val2)) -> val1*val2
    | _ -> failwith "Term provided is not a integer")
  | And (op1,op2) -> EBool( match ((eval env op1), (eval env op2)) with
    | (EBool(val1), EBool(val2)) -> val1 && val2
    | _ -> failwith "Term provided is not a boolean");;

let program_that_fails = LetFunIn("f","x",Plus(Bool true,Variable "y"),Apply(Variable("f"),Int(3)));;


let recursive_function = LetFunIn(
  "f",
  "x", 
  IfThenElse(
    Minus(
      Variable "x", 
      Int 2
    ),
    Int(1), 
    Plus(
      Variable "x", 
      Apply(
        Variable "f",
        Substraction(
          Variable "x", 
          Int 1
        )
      )
    )
  ), 
  Variable("f")
);;
print_endline (show_evt (eval emptyenv recursive_function));;

let apply_not_ending_recursive_function = LetFunIn(
  "g",
  "y", 
  Apply(
        Variable "g",
        Substraction(
          Variable "y", 
          Int 1
        )
      ), 
  Apply(
    Variable("g"),
    Int(5)
  )
);;

let static_program = LetIn(
  "f",
  LetIn(
    "a",
    Int(1), 
    Fun(
      "y", 
      Plus(
        Variable "y",
        Int(1)
      )
    )
  ),
  LetIn(
    "a",
    Int(2), 
    Apply(
      Variable "f", 
      Int(4)
    )
  )
);;

(* if you run this program, then it will never end*)
let res = eval emptyenv (apply_not_ending_recursive_function);;
print_endline (show_evt res);;