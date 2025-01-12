open MiniImpControlFlow

type label = int ;;

let keys_of_hashtable table =
  Hashtbl.fold (fun key _ acc -> key :: acc) table []
;;

let reversed_edges = Hashtbl.create 256;;
let reverse_edges edges = 
  let rec iter_list dest = function
  |[] -> reversed_edges
  | x:: elem' -> Hashtbl.add reversed_edges x dest; iter_list dest elem' in
  Hashtbl.iter (fun key value -> ignore (iter_list key value) ) edges ;;

(*
 Stuff to build inital set of registers
*)
module StringSet = Set.Make(struct
  type t = string
  let compare = compare
end);;
let print_set_as_list set =
  let elements = StringSet.elements set in
  String.concat "; " elements
;;


let find_or_empty_set table key = 
  try Hashtbl.find table key with
  | _ -> StringSet.empty;;


let initial_registers = ref StringSet.empty;;
let in_regs = Hashtbl.create 256;;
let out_regs = Hashtbl.create 256;;

let dv_in block =
  find_or_empty_set in_regs block;;
  
let dv_out block =
    find_or_empty_set out_regs block;;

let rec init_registers blocks nodes = 
  let add_registers = function
  | Assign(variable, _)-> 
    initial_registers := StringSet.add variable !initial_registers;
  | _ -> ignore ()
  in 
  let rec iterate_block b = 
    match b with
    | [] -> []
    | instruction :: b' -> add_registers instruction; iterate_block b'
    in
  (* Special register added statically *)
  initial_registers := StringSet.add "in" !initial_registers;
  match blocks with
  | [] -> ()
  | x :: lis' -> ignore (iterate_block (Hashtbl.find nodes x)); init_registers lis' nodes


(*
 Start of analysis
*)
let rec get_defined_variables block = 
  match block with
  | [] -> StringSet.empty
  | ins :: block' -> (match ins with
    | Assign(variable,_) -> 
      StringSet.add variable (get_defined_variables block')
    | _ -> get_defined_variables block'
  )
;;

let df_analysis (cfg : (label, statement list) Hashtbl.t * (label, label list) Hashtbl.t ) = 
  match cfg with
  |(nodes, edges) ->
    (
      let blocks = List.sort compare (keys_of_hashtable nodes) in 
      init_registers blocks nodes;
      (* init all sets *)
      let rec init_dv = function
      |[] -> ()
      | block:: blocks' -> Hashtbl.add in_regs block !initial_registers; Hashtbl.add out_regs block !initial_registers; init_dv blocks' in
      init_dv blocks;

      (* build hash table of reversed direction of the edges *)
      reverse_edges edges;
      Hashtbl.iter (fun x y -> Printf.printf "ID= [%d]  ->  [%d]\n" x y) reversed_edges; print_endline "--------";
      
      (* compute local dv_in and dv_out *)
      let rec scan_blocks = function
      | [] -> ()
      | block:: blocks' -> 
        (* 
          DV_IN STUFF 
        *)
        (
        print_endline "iterating";
        Printf.printf "%d\n Predecessors:\n" block;
        let predecessors = Hashtbl.find_all reversed_edges block in
        List.iter (Printf.printf "sos %d \n") predecessors;
        (match predecessors with
          (* if empty is initial block*)
        | [] -> Hashtbl.replace in_regs block (StringSet.add "in" StringSet.empty)
        | x:: pred' -> 
          let res_set = find_or_empty_set out_regs x in
          let rec scan_preds set = function
            |[] -> set
            |pred :: pred' -> scan_preds (StringSet.inter set (dv_out pred)) pred' in
          Hashtbl.replace in_regs block ( scan_preds res_set pred');
          Hashtbl.iter (fun x y -> Printf.printf "ID= [%d]  ->  [%s]\n" x (print_set_as_list y)) in_regs; print_endline "--------");
        
        
        print_endline "DVOUT:\n";
        (*
          DV_OUT STUFF
        *)
        Hashtbl.replace out_regs block (StringSet.union (dv_in block) (get_defined_variables (Hashtbl.find nodes block) ));
        Hashtbl.iter (fun x y -> Printf.printf "ID= [%d]  ->  [%s]\n" x (print_set_as_list y)) out_regs; print_endline "--------";
        scan_blocks blocks';
        )
        
      in
        let rec find_fix_point g_in g_out =
          if g_in = in_regs && g_out = out_regs then
            (g_in,g_out)
          else 
            let g_in = Hashtbl.copy in_regs in
            let g_out = Hashtbl.copy out_regs in
            scan_blocks blocks;
            find_fix_point g_in g_out 
          in
        find_fix_point (Hashtbl.create 0) (Hashtbl.create 0)
        
    )