type tree =
| Leaf of int
| Node of int * tree * tree;;


(* is this tail recursive? *)

let rec leaves_sum tree sum =
  match tree with
  |Leaf(value) -> sum + value
  |Node(_, left, right) -> ((leaves_sum [@tailcall]) left (leaves_sum right sum) ) ;; 


Printf.sprintf "%d" (leaves_sum (Node(5,Node(6,Leaf(5),Leaf(10)),Leaf(5))) 0);;