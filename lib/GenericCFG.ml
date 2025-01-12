module type CFG_Types  = sig
  type node
end;;

module GenericCFGImpl = functor (Types: CFG_Types) -> struct
  type label = int
  type node = Types.node
  type nodes = (label, node) Hashtbl.t
  type edges = (label,label list) Hashtbl.t
  type cfg = nodes * edges
  let nodes = Hashtbl.create 256

  let edges = Hashtbl.create 256
  let id = ref 0
  let add_node (node : node) = 
    id := !id + 1;
    Hashtbl.replace nodes !id node;
    !id
  let add_edge (node: label) (node_list: label list) = 
    Hashtbl.replace edges node node_list;
    edges
  let get_last_label () = !id

  let get_prec_label () = (!id-1)
  let find table id = 
    try Hashtbl.find table id
    
    with
    |_ -> failwith (Printf.sprintf "ID: %d not found" id);;

  let rec show_label_list list = 
    match list with
    | [] -> ""
    | elem::[] -> Printf.sprintf "%d" elem
    | elem:: list' -> Printf.sprintf "%d, %s" elem (show_label_list list');;
end

module VariableRegs = struct

  type register = string
  let variable_regs = 
    let table = Hashtbl.create 256 in
    (* add two special registers*)
    Hashtbl.add table "in" "in";
    Hashtbl.add table "out" "out"; 
    table
  let id = ref (-1)
  let get_next_register () = 
    id := !id + 1 ;
    Printf.sprintf "r%d" !id;;
  let peek_next_register () = 
    Printf.sprintf "r%d" (!id + 1);;
  
  let get_last_register () = 
    Printf.sprintf "r%d" !id;;
  let get_prec_register () = 
    Printf.sprintf "r%d" (!id-1);;
  
  let link_variable variable reg = 
    Hashtbl.replace variable_regs variable reg;;
  let get_reg_from_variable variable =
    try Hashtbl.find variable_regs variable with
    | _ -> failwith (Printf.sprintf "GenericCFG - get_ref_from_variable - Variabile %s not found" variable );;
  let get_next_reg_if_none variable =
    match Hashtbl.find_opt variable_regs variable with
    | Some(reg) -> reg
    | None -> get_next_register();;

  let get_reg_or_empty variable =
    match Hashtbl.find_opt variable_regs variable with
    | Some(reg) -> reg
    | None -> "";;

  let get_next_reg_if_none variable =
    match Hashtbl.find_opt variable_regs variable with
    | Some(reg) -> reg
    | None -> get_next_register();;
  
  let get_last_reg_if_none variable =
    match Hashtbl.find_opt variable_regs variable with
    | Some(reg) -> reg
    | None -> get_last_register();;
end

