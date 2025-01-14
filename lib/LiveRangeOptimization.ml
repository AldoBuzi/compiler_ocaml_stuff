open MiniRISC
open CFGDataFlowAnalysis

module LiveRangeSet = Set.Make(struct
  type t = label * label
  let compare = compare
end);;

let live_ranges : (string, LiveRangeSet.t) Hashtbl.t = Hashtbl.create 256;;

let compute_live_ranges live_set = function
  | (_,edges) -> 
    match live_set with
      | (live_in_tbl,live_out_tbl) -> 
      (
          let rec scan_out_regs source regs = 
          match regs with
          | [] -> ()
          | elem:: lis' -> 
            let out_edges = Hashtbl.find edges source in
            List.iter (function x -> 
              let live_in  = Hashtbl.find live_in_tbl x in
              let res = StringSet.exists (function regs -> regs = elem) live_in in
              if res then Hashtbl.replace live_ranges elem (LiveRangeSet.add (source, x ) (try Hashtbl.find live_ranges elem with | _ -> LiveRangeSet.empty));
            ) out_edges;
            scan_out_regs source lis'
        in
        Hashtbl.iter (fun source destination -> scan_out_regs source (StringSet.fold (fun elem acc -> elem :: acc) destination [] )) live_out_tbl;
      )
;;

let print_int_pair_set set =
  LiveRangeSet.iter (fun (a, b) ->
    Printf.printf "(%d, %d)\n" a b
  ) set
;;

let replace_regs _r1 _r2 merged_set nodes =
  let rec replace_regs_block _r1 _r2 block =
    match block with
    | [] -> []
    | ins:: block' -> 
      let new_ins =  (match ins with
        | Add(r1,r2,r3) -> Some ( Add(
          (if _r1 = r1 || _r2 = r1 then _r1 else r1),
          (if _r1 = r2 || _r2 = r2  then _r1 else r2),
          (if _r1 = r3 || _r2 = r3  then _r1 else r3)
          ))
        | Sub(r1,r2,r3) -> Some ( Sub(
          (if _r1 = r1 || _r2 = r1 then _r1 else r1),
          (if _r1 = r2 || _r2 = r2  then _r1 else r2),
          (if _r1 = r3 || _r2 = r3  then _r1 else r3)
          ))
        | Mult(r1,r2,r3) -> Some ( Mult(
          (if _r1 = r1 || _r2 = r1 then _r1 else r1),
          (if _r1 = r2 || _r2 = r2  then _r1 else r2),
          (if _r1 = r3 || _r2 = r3  then _r1 else r3)
          ))
        | And(r1,r2,r3) -> Some (And(
          (if _r1 = r1 || _r2 = r1 then _r1 else r1),
          (if _r1 = r2 || _r2 = r2  then _r1 else r2),
          (if _r1 = r3 || _r2 = r3  then _r1 else r3)
          ))
        | Less(r1,r2,r3)  -> Some (Less(
          (if _r1 = r1 || _r2 = r1 then _r1 else r1),
          (if _r1 = r2 || _r2 = r2  then _r1 else r2),
          (if _r1 = r3 || _r2 = r3  then _r1 else r3)
          ))
        
        | AddI(r1,v,r3) -> 
          (* if instruction it's like AddI r0, 0, r0; then just remove it, it's not useful *)
          let r1 = if _r1 = r1 || _r2 = r1 then _r1 else r1 in
          let r3 = if _r1 = r3 || _r2 = r3 then _r1 else r3 in
          if r1 = r3 && v = 0 then None 
          else Some ( AddI(r1,v,r3) )
        | SubI(r1,v,r3) -> 
          (* if instruction it's like SubI r0, 0, r0; then just remove it, it's not useful *)
          let r1 = if _r1 = r1 || _r2 = r1 then _r1 else r1 in
          let r3 = if _r1 = r3 || _r2 = r3 then _r1 else r3 in
          if r1 = r3 && v = 0 then None 
          else Some (SubI(r1,v,r3) )
        | MultI(r1,v,r3) ->
          (* if instruction it's like MultI r0, 1, r0; (a = a * 1) then just remove it, it's not useful *) 
          let r1 = if _r1 = r1 || _r2 = r1 then _r1 else r1 in
          let r3 = if _r1 = r3 || _r2 = r3 then _r1 else r3 in
          if r1 = r3 && v = 1 then None 
          else Some(MultI(r1,v,r3))
        | AndI(r1,v,r3) -> Some( AndI(
          (if _r1 = r1 || _r2 = r1 then _r1 else r1),
          v,
          (if _r1 = r3 || _r2 = r3  then _r1 else r3)
          ))
        | Not(r1,r3) -> Some(Not(
          (if _r1 = r1 || _r2 = r1 then _r1 else r1),
          (if _r1 = r3 || _r2 = r3  then _r1 else r3)
          ))
        | LoadI(v,r3) -> Some(LoadI(
          v,
          (if _r1 = r3 || _r2 = r3  then _r1 else r3)
          ))
        | Copy(r1,r3)  -> 
          (* if instruction it's like Copy r0, r0; then just remove it, it's not useful *)
          let r1 = if _r1 = r1 || _r2 = r1 then _r1 else r1 in
          let r3 = if _r1 = r3 || _r2 = r3 then _r1 else r3 in
          if r1 = r3 then None
          else
            Some(Copy(r1,r3))
        | _ -> Some(ins)
    ) in
    (* removed silly instruction that the user my write (note: this could have been done in previous phases of the compilation) *)
    match new_ins with
    | None -> replace_regs_block _r1 _r2 block'
    | Some(ins) ->  ins :: replace_regs_block _r1 _r2 block'
  in
  LiveRangeSet.iter (function (_,iend) -> 
    let new_block = replace_regs_block _r1 _r2 (Hashtbl.find nodes iend) in
    Hashtbl.replace nodes iend new_block;
    ) merged_set; 
;;

let live_range_optimization live_set = function
  |(nodes,edges) -> 
    compute_live_ranges live_set (nodes, edges);
    Hashtbl.iter (fun x y -> Printf.printf "ID= [%s]  ->\n " x; (print_int_pair_set y)) live_ranges; print_endline "--------";
    let rec check reg regs = 
      match regs with
      | [] -> None
      | x :: list' -> 
        let reg_set = try Some(Hashtbl.find live_ranges reg) with _ -> None in
        let x_set = try Some(Hashtbl.find live_ranges x) with _ -> None in
        (* if a register is not available, it means that it has already been merged in the current phase *)
        match (reg_set,x_set) with
        | (None, _) -> None
        | (_, None) -> check reg list' (* current reg may be merged with other registers, so I keep scanning *)
        | (Some(reg_set), Some(x_set)) ->
          let intersection = LiveRangeSet.inter reg_set x_set in
          if reg != x && LiveRangeSet.is_empty intersection then
            Some((reg,x), LiveRangeSet.union reg_set x_set)
          else check reg list'
    in
    let rec compute regs = 
      match regs with
      | [] -> ()
      | reg :: list' -> 
        (* I do not merge in and out *)
        if reg = "in" || reg = "out" then compute list'
        else 
          match check reg regs with
          | None -> compute list'
          | Some((r1,r2), set) -> 
            Hashtbl.remove live_ranges r1;
            Hashtbl.remove live_ranges r2;
            Hashtbl.add live_ranges r1 set;
            replace_regs r1 r2 set nodes;
    in
    (* iterate till no remaining register can be merged *)
    let rec merge_regs merge_set = 
      if merge_set = live_ranges then ()
      else 
        (* HEURISTIC *)
        (* sort regs by increasing number of edges *)
        (compute (List.sort (fun x y -> 
          compare
            (LiveRangeSet.cardinal (Hashtbl.find live_ranges x))  
            (LiveRangeSet.cardinal (Hashtbl.find live_ranges y)) 
          ) 
          (keys_of_hashtable live_ranges));
        merge_regs (Hashtbl.copy live_ranges);)
    in
    merge_regs (Hashtbl.create 0);
    Hashtbl.iter (fun x y -> Printf.printf "ID= [%s]  ->\n " x; (print_int_pair_set y)) live_ranges; print_endline "--------";
    (nodes,edges)
;;
      