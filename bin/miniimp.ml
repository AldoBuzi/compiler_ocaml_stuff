
  (*
   Things for abstract syntax tree
  *)
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

  
  (*let emptyenv = function _ -> Unbound;;
  let bind (s: integers env) (x: variable) (v: integers) = 
  function (i: variable) -> if (i = x) then v else (s i)*)