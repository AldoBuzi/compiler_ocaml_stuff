type integers = int [@@deriving show];;


(*
 Added labels to nodes
 Map for nodes/edges?
 Save input x and ouput y 
*)

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
|Smaller of ops * ops  [@@deriving show];;

type command = 
|Skip
|Assign of variable * ops 
|CommandSeq of command * command
|IfThenElse of boolean * command * command
|WhileDo of boolean * command [@@deriving show];;


type program = 
|Program of variable * variable * command;;

(* EXPOSE ONLY eval *)
(*type 't env = variable -> 't;;*)
(*val my_hash : (variable, integers) Hashtbl.t*)

(*val emptyenv: variable -> 'a*)
(*val bind: (variable, integers) Hashtbl.t -> variable -> integers -> (variable, integers) Hashtbl.t
val eval_ops : (variable, integers) Hashtbl.t -> ops -> int
val eval_bool : (variable, integers) Hashtbl.t -> boolean -> bool
val eval_command: (variable, integers) Hashtbl.t -> command -> (variable, integers) Hashtbl.t*)
val eval: program -> integers -> integers
