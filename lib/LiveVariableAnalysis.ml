open CFGDataFlowAnalysis
open MiniRISC


let rec get_defined_variables block = 
  match block with
  | [] -> StringSet.empty
  | ins :: block' -> (match ins with
    | Add(r1,r2,_) | Sub(r1,r2,_) | Mult(r1,r2,_) | And(r1,r2,_) | Less(r1,r2,_) -> 
      StringSet.add r1 (StringSet.add r2 (get_defined_variables block'))
    | AddI(r1,_,_) | SubI(r1,_,_) | MultI(r1,_,_) | AndI(r1,_,_) | Not(r1,_) | Copy(r1,_) | Load(r1,_) | CJump(r1,_,_) -> 
      StringSet.add r1 (get_defined_variables block')
    | _ -> get_defined_variables block'
  )
;;

let get_used_variables block = 
  let rec compute block defined_regs =
  match block with
  | [] -> StringSet.empty
  | ins :: block' -> (match ins with
    | Add(r1,r2,r3) | Sub(r1,r2,r3) | Mult(r1,r2,r3) | And(r1,r2,r3) | Less(r1,r2,r3) -> (
      let used_regs = StringSet.diff (StringSet.add r1 (StringSet.add r2 StringSet.empty)) defined_regs in
      StringSet.union used_regs (compute block' (StringSet.add r3 defined_regs)))
    | AddI(r1,_,r3) | SubI(r1,_,r3) | MultI(r1,_,r3) | AndI(r1,_,r3) | Not(r1,r3) | Copy(r1,r3) | Load(r1,r3) -> 
      let used_regs = StringSet.diff (StringSet.add r1 StringSet.empty) defined_regs in
      StringSet.union used_regs (compute block' (StringSet.add r3 defined_regs))
    | LoadI(_,r3) -> compute block' (StringSet.add r3 defined_regs)
    | Store(r3,r4) -> compute block' (StringSet.add r4 (StringSet.add r3 defined_regs))
    | CJump(r1,_,_) -> 
      let used_regs = StringSet.diff (StringSet.add r1 StringSet.empty) defined_regs in
      StringSet.union used_regs (compute block' defined_regs)
    | _ -> compute block' defined_regs
  ) in 
  compute block StringSet.empty
;;

let live_analysis (cfg : (int, comm list) Hashtbl.t * (int, int list) Hashtbl.t )  =
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
