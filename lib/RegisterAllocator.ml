open MiniRISC

(* start mem address from 10000 to make debugging easier *)
let current_mem_addr = ref 10000;;
let get_next_memory_address () =
  current_mem_addr := !current_mem_addr + 4;
  !current_mem_addr
;;
let reg_to_mem = Hashtbl.create 256 ;;

let get_mem_addr_for_register reg = 
  try
    Some (Hashtbl.find reg_to_mem reg)
  with
  (* if none, it means that the register must not be spilled to memory *)
  |_ -> None;;

(* mapped later to physical registers *)
let (reg_a, reg_b) = ("R_a", "R_b");;

let add_memory_ops instruction is_guard = 
  match instruction with
    | Add(r1,r2,r3) |Sub(r1,r2,r3)| Mult(r1,r2,r3) | And(r1,r2,r3) | Less(r1,r2,r3) -> 
      let (r1_ins,r1) = match get_mem_addr_for_register r1 with
        |None -> ([],r1)
        |Some(addr) -> ([LoadI(addr, reg_a); Load(reg_a, reg_a)],reg_a) in
      let (r2_ins,r2) = match get_mem_addr_for_register r2 with
        |None -> ([],r2)
        |Some(addr) -> ([LoadI(addr, reg_b); Load(reg_b, reg_b)],reg_b) in
      (* r3 contains register destination for add *)
      let (r3_ins,r3) = match get_mem_addr_for_register r3 with
        |None -> ([],r3)
        |Some(addr) -> ((if is_guard then [] else [LoadI(addr, reg_a); Store(reg_b,reg_a)]),reg_b) in
      r1_ins @ 
      r2_ins @ (
        (match instruction with
          | Add(_) -> Add(r1,r2,r3)
          | Sub(_) -> Sub(r1,r2,r3)
          | Mult(_) -> Mult(r1,r2,r3)
          | And(_) -> And(r1,r2,r3)
          | Less(_) -> Less(r1,r2,r3)
          | _ -> failwith "Impossibile case"
        ) :: 
      r3_ins
      )
    | AddI(r1,_,r3) | SubI(r1,_,r3) | MultI(r1,_,r3) | AndI(r1,_,r3)| Copy(r1,r3) | Not(r1,r3) -> 
      let (r1_ins,r1) = match get_mem_addr_for_register r1 with
        |None -> ([],r1)
        |Some(addr) -> ([LoadI(addr, reg_a); Load(reg_a, reg_a)],reg_a) in
      (* r3 contains register destination for add *)
      let (r3_ins,r3) = match get_mem_addr_for_register r3 with
        |None -> ([],r3)
        |Some(addr) -> ((if is_guard then [] else [LoadI(addr, reg_b); Store(reg_a,reg_b)]),reg_a) in
      r1_ins @ 
      (
        (match instruction with
          | AddI(_,v,_) -> AddI(r1,v,r3)
          | SubI(_,v,_) -> SubI(r1,v,r3)
          | MultI(_,v,_) -> MultI(r1,v,r3)
          | AndI(_,v,_) -> AndI(r1,v,r3)
          | Copy(_) -> Copy(r1,r3)
          | Not(_) -> Not(r1,r3)
          | _ -> failwith "Impossibile case"
        ) :: 
      r3_ins
      )
    | LoadI(v,r1) -> 
      (match get_mem_addr_for_register r1 with
        |None -> [instruction]
        |Some(addr) -> [LoadI(addr, reg_a); LoadI(v, reg_b); Store(reg_b, reg_a)])
    (* Nop, Load, Store, Jump and CJump will fall in this case *)
    | _ -> [instruction]
  ;;
let rec spill_block block = 
  match block with
  | [] -> []
  | instruction :: [] -> 
    (* the last instruction of each block must be a instruction that writes a temporary register to decide the correct branch *)
    (* Therefore no load and store are added in this case *)
    (add_memory_ops instruction) true
  | instruction :: block' -> 
    (add_memory_ops instruction false) @ spill_block block'
;;
let compute_frequencies nodes = 
  let frequencies = Hashtbl.create 256 in
  let rec compute_block = function
    |[] -> ()
    |ins :: block' -> 
      let new_ins =  (match ins with
          | Add(r1,r2,r3) | Sub(r1,r2,r3)| Mult(r1,r2,r3) | And(r1,r2,r3) | Less(r1,r2,r3)  -> [r1;r2;r3]
          | AddI(r1,_,r3) | SubI(r1,_,r3) | MultI(r1,_,r3) | AndI(r1,_,r3) | Not(r1,r3) | Copy(r1,r3) -> [r1;r3]
          | LoadI(_,r3) -> [r3]
          (* Nop, Load, Store, Jump and CJump will fall in this case *)
          | _ -> []
      )
     in
      (* update frequencies of registers *)
      List.iter (function x -> Hashtbl.replace frequencies x ((try Hashtbl.find frequencies x with _ -> 0) +1) ) new_ins;
      compute_block block'
  in
  Hashtbl.iter (fun _ block -> compute_block block ) nodes;
  (* sort by ascending number of occurences*)
  List.sort (fun (_,occ) (_,occ2) ->  compare occ occ2 ) (Hashtbl.fold (fun key value acc -> (key, value) :: acc) frequencies [])
;;

let register_allocator target_number = function
  | (nodes, edges) ->
    (* keep two registers for temporary values, say Ra and Rb *)
    let actual_target = target_number - 2 in
    (* HEURISTIC: Always keep "in", "out" and then select the n - 4 most used registers *)
    (* For example, with 6 registers:  
      If I have in, r1,r2,r3,r4,r5,r6,r7,r8, out vritual registers
      I keep: in, out, virtual r8 assigned to r1, virtual r1 assigned to r2 and then r3, r4 (phyisical) reserved for spilling values
      Virtual registers r2,r3,r4,r5,r6,r7 go to memory (in this example r8 and r1 have max # of occurrences)
      Note: I keep as spelling registers always the last two phyisical registers
      Note: The mapping from virtual registers to physical ones is done at the bottom of this file
     *)
    let frequencies = compute_frequencies nodes in
    let number_of_regs = List.length frequencies in
    let rec populate_reg_to_mem frequencies iter =
      match frequencies with
      | [] -> iter
      | (reg,_) :: frequencies' -> 
        (* spill to memory the first number_of_regs - actual_target registers and then stop *)
        if iter = number_of_regs - actual_target then iter
        else if reg = "in" || reg = "out" then populate_reg_to_mem frequencies' iter 
        else (
          Hashtbl.add reg_to_mem reg (get_next_memory_address ());
          populate_reg_to_mem frequencies' (iter + 1)
          )
    in
    Printf.printf "Spilled to memory %d variables\n" (number_of_regs - actual_target);
    if actual_target < 4 then failwith "RegisterAllocator: impossible to compile the program with fewer than 4 registers"
    else if number_of_regs  <= actual_target then
      (nodes, edges) (* nothing to be done, all values can reside in the registers *)
    else (
      let assignment = populate_reg_to_mem frequencies 0 in
      (* if we fall there, we have to spill values to memory *)
      Hashtbl.iter (fun node_id block -> Hashtbl.replace nodes node_id (spill_block block)) nodes; 

      (* 
        Translate virtual registers to physical ones 
        Translate R_a and R_b to last two physical register
      *)
      (* get list of all physical registers *)
      (* if target is 4, then we have four registers named as: in, out, r1, r2 *)
      let physical_registers = List.init (target_number-2) (fun i -> "r" ^ string_of_int (i + 1)) in
      let last_phys_assigned = ref (-1) in
      let virt_to_phys = Hashtbl.create 256 in
      (* initialize mapping with precomputed assignment *)
      Hashtbl.add virt_to_phys "in" "in";
      Hashtbl.add virt_to_phys "out" "out";
      Hashtbl.add virt_to_phys reg_a (List.nth physical_registers (actual_target - 2));
      Hashtbl.add virt_to_phys reg_b (List.nth physical_registers (actual_target - 1));
      let find_assigned_physical virtual_reg = 
        try Hashtbl.find virt_to_phys virtual_reg with
        |_ ->
          (* get next available physical register *)
          last_phys_assigned := !last_phys_assigned + 1;            
          if !last_phys_assigned >= actual_target - 2 then failwith ("RegisterAllocator - mapping virtual to physical - Tried assigning reserved register, this must not happen" ^ (Printf.sprintf "thrown by virtual register %s: tried %d" virtual_reg !last_phys_assigned ))
          else
          let physical_register = List.nth physical_registers !last_phys_assigned in
          Hashtbl.add virt_to_phys virtual_reg physical_register;
          physical_register
      in
      let rec fix_block = function
      |[] -> []
      | instruction:: block' -> 
        let new_ins = (match instruction with
          | Add(r1,r2,r3) |Sub(r1,r2,r3)| Mult(r1,r2,r3) | And(r1,r2,r3) | Less(r1,r2,r3) -> 
            let phys_r1 = find_assigned_physical r1 in
            let phys_r2 = find_assigned_physical r2 in
            let phys_r3 = find_assigned_physical r3 in
            (match instruction with
              | Add(_) -> Add(phys_r1,phys_r2,phys_r3)
              | Sub(_) -> Sub(phys_r1,phys_r2,phys_r3)
              | Mult(_) -> Mult(phys_r1,phys_r2,phys_r3)
              | And(_) -> And(phys_r1,phys_r2,phys_r3)
              | Less(_) -> Less(phys_r1,phys_r2,phys_r3)
              | _ -> failwith "Impossibile case"
            )
          | AddI(r1,_,r3) | SubI(r1,_,r3) | MultI(r1,_,r3) | AndI(r1,_,r3) | Not(r1,r3) | Copy(r1,r3) | Load(r1,r3) | Store(r1,r3)  ->
            let phys_r1 = find_assigned_physical r1 in
            let phys_r3 = find_assigned_physical r3 in
            (match instruction with
              | AddI(_,v,_) -> AddI(phys_r1,v,phys_r3)
              | SubI(_,v,_) -> SubI(phys_r1,v,phys_r3)
              | MultI(_,v,_) -> MultI(phys_r1,v,phys_r3)
              | AndI(_,v,_) -> AndI(phys_r1,v,phys_r3)
              | Not(_) -> Not(phys_r1,phys_r3)
              | Copy(_) -> Copy (phys_r1,phys_r3)
              | Load(_) -> Load (phys_r1,phys_r3)
              | Store(_) -> Store (phys_r1,phys_r3)
              | _ -> failwith "Impossibile case"
            )
          | LoadI(v, r3) ->
            let phys_r3 = find_assigned_physical r3 in
            LoadI(v,phys_r3)
          (* there fall Nop, Jump and Cjump *)
          |_ -> instruction
        ) in
        new_ins :: fix_block block'
      in
      Hashtbl.iter (fun node_id block -> 
        let new_block = fix_block block in
        Hashtbl.replace nodes node_id new_block
        ) nodes;
      (nodes,edges)
    )


