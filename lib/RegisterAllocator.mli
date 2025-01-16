open MiniRISC

val register_allocator : int -> ('a, comm list) Hashtbl.t * ('a, 'b list) Hashtbl.t -> ('a, comm list) Hashtbl.t * ('a, 'b list) Hashtbl.t