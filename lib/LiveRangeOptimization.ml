open MiniRISC
open CFGDataFlowAnalysis

module LiveRangeSet = Set.Make(struct
  type t = label * label
  let compare = compare
end);;

let live_ranges : (string, LiveRangeSet.t) Hashtbl.t = Hashtbl.create 256;;

let hashtbl_equal h1 h2 =
  let hashtbl_to_sorted_list h =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []
    |> List.sort (fun (x,_) (y,_) -> compare x y ) in
    let rec compare_equal h1 h2 =
    match h1, h2 with
    | [],[] -> true
    | x::h1', y::h2' -> 
      let c_equal = (fun (x,set1) (y,set2) -> x = y && LiveRangeSet.compare set1 set2 = 0  ) in
      c_equal x y && compare_equal h1' h2'
    | _ -> false (* if they have, for example, different length they are not equal*)
  in
  compare_equal (hashtbl_to_sorted_list h1) (hashtbl_to_sorted_list h2)
;;


let compute_live_ranges live_set edges = 
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
              let res = StringSet.exists (function _regs -> _regs = elem) live_in in
              if res then Hashtbl.replace live_ranges elem (LiveRangeSet.add (source, x) (try Hashtbl.find live_ranges elem with | _ -> LiveRangeSet.empty));
            ) out_edges;
            scan_out_regs source lis'
        in
        Hashtbl.iter (fun source destination -> scan_out_regs source (StringSet.fold (fun elem acc -> elem :: acc) destination [] )) live_out_tbl;
      )
;;


let replace_regs _r1 _r2 nodes =
  (* I merge _r1 and _r2 always in _r1 *)
  let rec replace_regs_block _r1 _r2 block =
    match block with
    | [] -> []
    | ins:: block' -> 
      let new_ins =  (match ins with
        | Add(r1,r2,r3) | Sub(r1,r2,r3) | Mult(r1,r2,r3) | And(r1,r2,r3) | Less(r1,r2,r3) ->
          let r1,r2,r3 = (if _r2 = r1 then _r1 else r1),(if _r2 = r2  then _r1 else r2), (if _r2 = r3  then _r1 else r3) in
          Some ( match ins with
            |Add(_) -> Add(r1,r2,r3)
            |Sub(_) -> Sub(r1,r2,r3)
            |Mult(_) -> Mult(r1,r2,r3)
            |And(_) -> And(r1,r2,r3)
            |Less(_) -> Less(r1,r2,r3)
            |_ -> failwith "impossible case"
          )
        | AddI(r1,v,r3) | SubI(r1,v,r3) | MultI(r1,v,r3) | AndI(r1,v,r3) -> 
          let r1 = if _r2 = r1 then _r1 else r1 in
          let r3 = if _r2 = r3 then _r1 else r3 in
          (* and case is required to avoid removing it *)
          (* if instruction is like AddI r0, 0, r0; then I just remove it, it's not useful *)
          (* if instruction is like SubI r0, 0, r0; then I just remove it, it's not useful *)
          if r1 = r3 && v = 0 && ins != MultI(r1,v,r3) && ins != AndI(r1,v,r3) then None 
          (* if instruction is like MultI r0, 1, r0; (a = a * 1) then I just remove it, it's not useful *) 
          else if r1 = r3 && v = 1 && ins = MultI(r1,v,r3) && ins != AndI(r1,v,r3) then None 
          else Some ( match ins with
            |AddI(_,v,_) -> AddI(r1,v,r3)
            |SubI(_,v,_) -> SubI(r1,v,r3)
            |MultI(_,v,_) -> MultI(r1,v,r3)
            |AndI(_,v,_) -> AndI(r1,v,r3)
            |_ -> failwith "impossible case" 
          )
        | Not(r1,r3) -> Some(Not(
          (if _r2 = r1 then _r1 else r1),
          (if _r2 = r3  then _r1 else r3)
          ))
        | LoadI(v,r3) -> Some(LoadI(
          v,
          (if _r2 = r3  then _r1 else r3)
          ))
        | Copy(r1,r3)  -> 
          (* if instruction it's like Copy r0, r0; then just remove it, it's not useful *)
          let r1 = if _r2 = r1 then _r1 else r1 in
          let r3 = if _r2 = r3 then _r1 else r3 in
          if r1 = r3 then None
          else
            Some(Copy(r1,r3))
        (* There fall cases: Nop, Load, Store, CJump, Jump *)
        | _ -> Some(ins)
    ) in
    match new_ins with
    | None -> replace_regs_block _r1 _r2 block'
    | Some(ins) ->  ins :: replace_regs_block _r1 _r2 block'
  in
  Hashtbl.iter (fun node_id block -> 
      let new_block =  replace_regs_block _r1 _r2 block in
      Hashtbl.replace nodes node_id new_block;
    ) nodes;
;;

let live_range_optimization live_set = function
  |(nodes,edges) -> 
    compute_live_ranges live_set edges;

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
          if reg != x && LiveRangeSet.is_empty (LiveRangeSet.inter reg_set x_set) then
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
            replace_regs r1 r2 nodes;
            compute list';
    in
    (* iterate till no remaining register can be merged *)
    let rec merge_regs merge_set = 
      if hashtbl_equal merge_set live_ranges then ()
      else 
        (* HEURISTIC *)
        (* sort regs by increasing number of edges *)
        let copy = Hashtbl.copy live_ranges in
        let res  = (keys_of_hashtable live_ranges) in
        print_endline "\n[";
        List.iter (fun x -> Printf.printf "%s, " x ) res;
        print_endline "]\n";
        (compute (List.sort (fun x y -> 
          compare
            (LiveRangeSet.cardinal (Hashtbl.find live_ranges x))  
            (LiveRangeSet.cardinal (Hashtbl.find live_ranges y)) 
          ) 
          (keys_of_hashtable live_ranges));
        merge_regs copy;)
    in
    merge_regs (Hashtbl.create 0);
    Hashtbl.iter (fun key value -> Printf.printf "%s -> %s\n" key (LiveRangeSet.fold (fun (x,y) acc -> "("^(string_of_int x)^ "," ^ (string_of_int y) ^")," ^ acc ) value "") ) live_ranges;
    (nodes,edges)
;;
      