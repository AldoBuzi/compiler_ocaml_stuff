open MiniRISC
open MiniImpControlFlow

(* utility to print human readable cfg *)
val hr_risc_graph: (int, comm list) Hashtbl.t -> (int, int list) Hashtbl.t -> unit

val mini_risc_cfg: (int, statement list) Hashtbl.t
* (int, int list) Hashtbl.t ->
(int, comm list) Hashtbl.t * (int, int list) Hashtbl.t