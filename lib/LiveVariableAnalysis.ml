open CFGDataFlowAnalysis
open MiniImpControlFlow

let live_analysis (cfg : (label, statement list) Hashtbl.t * (label, label list) Hashtbl.t )  =
  match cfg with
    |(nodes, edges) ->
      let blocks = List.sort (fun x y -> compare y x) (keys_of_hashtable nodes) in
      let l_dv_in block = 
        let defined_vars = get_defined_variables (Hashtbl.find nodes block) in
        let defined_vars = if List.hd (List.rev blocks) = block then StringSet.add "in" defined_vars else defined_vars in
        Hashtbl.replace in_regs block (
          StringSet.union 
            (get_used_variables (Hashtbl.find nodes block))  
            (StringSet.diff 
              (dv_out block) 
              defined_vars
            ) 
        );
      in
      let l_dv_out block = 
        let successors = Hashtbl.find edges block in
        (match successors with
          (* if empty is final block*)
        | [] -> Hashtbl.replace out_regs block (StringSet.add "out" StringSet.empty)
        | x:: succs' -> 
          let res_set = find_or_empty_set in_regs x in
          let rec scan_succs set = function
            |[] -> set
            |succ :: succ' -> 
              scan_succs (StringSet.union set (dv_in succ)) succ' 
          in
          Hashtbl.replace out_regs block ( scan_succs res_set succs');
        )
      in
      CFGDataFlowAnalysis.df_analysis blocks StringSet.empty l_dv_in l_dv_out
