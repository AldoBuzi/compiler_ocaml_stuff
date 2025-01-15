open MiniImp
type statement = 
|Skip
|Assign of variable * ops
|Guard of boolean [@@deriving show];;

(* utility to print human readable cfg *)
val hr_graph : (int, statement list) Hashtbl.t -> (int, int list) Hashtbl.t -> unit
val build_cfg : command -> (int, statement list) Hashtbl.t * (int, int list) Hashtbl.t