type variable = string [@@deriving show];;

type term =
  |Int of int
  |Bool of bool
  |Variable of variable
  |Fun of variable * term
  |Apply of term * term
  |Plus of term * term
  |Minus of term * term
  |Times of term * term
  |Smaller of term * term
  |Greater of term * term
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

val emptyenv : variable -> 'a

val eval: evt env -> term -> evt