open CFGDataFlowAnalysis
open MiniRISC


let get_used_variables block initial_set = 
  let rec compute block defined_regs =
  match block with
  | [] -> (StringSet.empty, defined_regs)
  | ins :: block' -> (match ins with
    | Add(r1,r2,r3) | Sub(r1,r2,r3) | Mult(r1,r2,r3) | And(r1,r2,r3) | Less(r1,r2,r3) -> (
      let used_regs = StringSet.diff (StringSet.add r1 (StringSet.add r2 StringSet.empty)) defined_regs in
      let (new_used_set, new_defined_set) = (compute block' (StringSet.add r3 defined_regs)) in
      (StringSet.union used_regs new_used_set,  new_defined_set) )
    | AddI(r1,_,r3) | SubI(r1,_,r3) | MultI(r1,_,r3) | AndI(r1,_,r3) | Not(r1,r3) | Copy(r1,r3) -> 
      let used_regs = StringSet.diff (StringSet.add r1 StringSet.empty) defined_regs in
      let (new_used_set, new_defined_set) = (compute block' (StringSet.add r3 defined_regs)) in
      (StringSet.union used_regs new_used_set,  new_defined_set)
    | LoadI(_,r3) -> compute block' (StringSet.add r3 defined_regs)
    (* Nop, Load, Store, CJump and jump fall there *)
    | _ -> compute block' defined_regs
  ) in 
  compute block initial_set
;;

let live_analysis (cfg : (int, comm list) Hashtbl.t * (int, int list) Hashtbl.t )  =
  match cfg with
    |(nodes, edges) ->
      let blocks = List.sort (fun x y -> compare y x) (keys_of_hashtable nodes) in
      let l_dv_in block = 
        let initial_set = if List.hd (List.rev blocks) = block then StringSet.add "in" StringSet.empty else StringSet.empty in
        let (used_vars, defined_vars) = (get_used_variables (Hashtbl.find nodes block) initial_set)  in
        Hashtbl.replace in_regs block (
          StringSet.union 
          used_vars
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
