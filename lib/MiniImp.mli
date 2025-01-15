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
|Smaller of ops * ops  [@@deriving show];;

type command = 
|Skip
|Assign of variable * ops 
|CommandSeq of command * command
|IfThenElse of boolean * command * command
|WhileDo of boolean * command [@@deriving show];;


type program = 
|Program of variable * variable * command;;

val eval: program -> integers -> integers
