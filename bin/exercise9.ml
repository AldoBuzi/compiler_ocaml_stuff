type tree =
| Leaf of int
| Node of int * tree * tree;;


(* is this tail recursive? *)

let rec leaves_sum tree sum =
  match tree with
  |Leaf(value) -> sum + value
  |Node(_, left, right) -> ((leaves_sum [@tailcall]) left (leaves_sum right sum) ) ;; 

(* How can I make it tail recursive? Maybe it works if I represent the tree as a list*)
  (*
let rec leaves_sum = function
|Leaf(value) -> value
|Node(_, left,right) -> recursive_f left right 0
and recursive_f left right sum = 
  match (left,right) with
  | (Leaf(val1),Leaf(val2)) -> sum + val1 + val2
  | (Leaf(val1),Node(_, left, right)) -> ((recursive_f [@tailcall]) left right (sum + val1))
  | (Node(_, left, right),Leaf(val1)) -> ((recursive_f [@tailcall]) left right (sum + val1))
  | (Node(_, left, right),Node(_, left1, right1)) -> ((recursive_f [@tailcall]) left1 right1 ((recursive_f) left right sum));;*)

Printf.sprintf "%d" (leaves_sum (Node(5,Node(6,Leaf(5),Leaf(10)),Leaf(5))) 0);;