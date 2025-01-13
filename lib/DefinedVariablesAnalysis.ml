open CFGDataFlowAnalysis
open MiniImpControlFlow



let reversed_edges = Hashtbl.create 256;;
let reverse_edges edges = 
  let rec iter_list dest = function
  |[] -> reversed_edges
  | x:: elem' -> Hashtbl.add reversed_edges x dest; iter_list dest elem' in
  Hashtbl.iter (fun key value -> ignore (iter_list key value) ) edges ;;



let initial_variables = ref StringSet.empty;;


let rec init_variables_set blocks nodes = 
  let add_variables = function
  | Assign(variable, _)-> 
    initial_variables := StringSet.add variable !initial_variables;
  | _ -> ignore ()
  in 
  let rec iterate_block b = 
    match b with
    | [] -> []
    | instruction :: b' -> add_variables instruction; iterate_block b'
    in
  (* Special variable added statically *)
  initial_variables := StringSet.add "in" !initial_variables;
  match blocks with
  | [] -> ()
  | x :: lis' -> ignore (iterate_block (Hashtbl.find nodes x)); init_variables_set lis' nodes


let rec get_defined_variables block = 
  match block with
  | [] -> StringSet.empty
  | ins :: block' -> (match ins with
    | Assign(variable,_) -> 
      StringSet.add variable (get_defined_variables block')
    | _ -> get_defined_variables block'
  )
;;
(*
 Start of analysis
*)

let defined_variables_analysis (cfg : (label, statement list) Hashtbl.t * (label, label list) Hashtbl.t ) = 
  match cfg with
  |(nodes, edges) ->
    (
      let blocks = List.sort compare (keys_of_hashtable nodes) in 
      init_variables_set blocks nodes;

      (* build hash table of reversed direction of the edges *)
      reverse_edges edges;
      let l_dv_out block = 
        (*
          DV_OUT STUFF
        *)
        Hashtbl.replace out_regs block (StringSet.union (dv_in block) (get_defined_variables (Hashtbl.find nodes block) ));
      in
      let l_dv_in block = 
        let predecessors = Hashtbl.find_all reversed_edges block in
        (match predecessors with
          (* if empty is initial block*)
        | [] -> Hashtbl.replace in_regs block (StringSet.add "in" StringSet.empty)
        | x:: pred' -> 
          let res_set = find_or_empty_set out_regs x in
          let rec scan_preds set = function
            |[] -> set
            |pred :: pred' -> scan_preds (StringSet.inter set (dv_out pred)) pred' in
          Hashtbl.replace in_regs block ( scan_preds res_set pred');
        )
      in
      CFGDataFlowAnalysis.df_analysis blocks !initial_variables l_dv_in l_dv_out
    )