open MiniRISC
open MiniImpControlFlow
open GenericCFG
open GenericCFGImpl(struct type node = comm list end);;
open VariableRegs;;

type print_list = comm list list [@@deriving show];;

(* Utility to get human readable representation of cfg, thus making debugging easier *) 
let hr_risc_graph nodes edges = 
  let translate_risc risc = 
    match risc with
    | Nop -> "\tNop"
    | Add(r1,r2,r3) -> Printf.sprintf "\tadd %s %s => %s" r1 r2 r3
    | Sub(r1,r2,r3) -> Printf.sprintf "\tsub %s %s => %s" r1 r2 r3
    | Mult(r1,r2,r3) -> Printf.sprintf "\tmult %s %s => %s" r1 r2 r3
    | And(r1,r2,r3) -> Printf.sprintf "\tand %s %s => %s" r1 r2 r3
    | Less(r1,r2,r3) -> Printf.sprintf "\tless %s %s => %s" r1 r2 r3
    | AddI(r1,i,r3) -> Printf.sprintf "\taddi %s %d => %s" r1 i r3
    | SubI(r1,i,r3) -> Printf.sprintf "\tsubi %s %d => %s" r1 i r3
    | MultI(r1,i,r3) -> Printf.sprintf "\tmulti %s %d => %s" r1 i r3
    | AndI(r1,i,r3) -> Printf.sprintf "\tandi %s %d => %s" r1 i r3
    | Not(r1,r2) -> Printf.sprintf "\tnot %s => %s" r1 r2
    | Copy(r1,r2) -> Printf.sprintf "\tcopy %s => %s" r1 r2
    | Load(r1,r2) -> Printf.sprintf "\tload %s => %s" r1 r2
    | LoadI(i,r2) -> Printf.sprintf "\tloadi %d => %s" i r2
    | _ -> failwith "translate_risc: not mapped yet" in
  let rec get_node_representation node = 
    match node with
    | [] -> ""
    | elem::remaining' -> Printf.sprintf "%s\n%s" (translate_risc elem) (get_node_representation remaining') in
  Hashtbl.iter (fun x y -> Printf.printf "ID = %d: [\n%s\t]  ->  [%s]\n\n" x (get_node_representation y) (try show_label_list (Hashtbl.find edges x) with |_ -> "")) nodes; print_endline "--------";
  print_endline "----------------------------------- \n EDGES:";
  Hashtbl.iter (fun x y -> Printf.printf "ID= [%d]  ->  [%s]\n" x (show_label_list y)) edges; print_endline "--------";
  ;;

let evaluate_ops op r1 r2 r3 =
  match op with
  | MiniImp.Plus(Constant(v1),Constant(v2)) -> [LoadI(v1, r1); AddI(r1, v2,r3 )]
  | Plus(Variable(_),Constant(v)) | Plus(Constant(v), Variable(_)) -> [AddI(r1, v,r3 )]
  | Plus(_,Constant(v)) | Plus(Constant(v), _) -> [AddI(r1, v,r3 )]
  | Plus(_,_) -> [Add(r1,r2,r3)]
  
  | Minus(Constant(v1),Constant(v2)) -> [LoadI(v1, r1); SubI(r1, v2,r3 )]
  | Minus(Variable(_),Constant(v)) | Minus(Constant(v), Variable(_)) -> [SubI(r1, v,r3 )]
  | Minus(_,Constant(v)) | Minus(Constant(v), _) -> [SubI(r1, v,r3 )]
  | Minus(_,_) -> [Sub(r1,r2,r3)]

  | Times(Constant(v1),Constant(v2)) -> [LoadI(v1, r1); MultI(r1, v2,r3 )]
  | Times(Variable(_),Constant(v)) | Times(Constant(v), Variable(_)) -> [MultI(r1, v,r3 )]
  | Times(_,Constant(v)) | Times(Constant(v), _) -> [MultI(r1, v,r3 )]
  | Times(_, _) -> [Mult(r1,r2,r3)]
  | _ -> failwith "evaluate_ops : This case wasn't supposed to happen";;
;;
let _NONE = "";;
let no_reg = fun () -> _NONE;;
let no_peek = no_reg;;
let rec translate_node node = 
  let rec translate_ops ops write_to_reg peek_write_reg = 
    match ops with
    (* Example x := 0 *)
    | MiniImp.Constant(t1) -> 
        let res = if peek_write_reg() = "" then get_next_register() else write_to_reg() in
        [LoadI(t1, res)]
    (* Example y:= 1; x := y *)
    | Variable(t1) -> 
      let t1_reg = get_reg_from_variable t1 in 
      let res = if peek_write_reg() = "" then get_next_register() else write_to_reg() in
      [Load(t1_reg, res)]
    | Plus(t1,t2) | Minus(t1,t2) | Times(t1,t2) -> (match (t1,t2) with
      (* Example x := 1 + 2 *)
      (* This will generate (loadi and addi) or (loadi and multi), etc. *)
      | (Constant(_), Constant(_)) -> 
          (* second parameter not required *)
          let r1 = get_next_register () in
          (* optimize registers use by using current temporary reg if variabile is null *)
          let reg_result = if peek_write_reg() = _NONE then r1 else write_to_reg() in
          evaluate_ops ops r1 _NONE reg_result
      (* Example y:= 1; x := y + 1 *)
      (* this kind of condition will generate an addi, multi etc. for sure *)
      | (Variable(t1), Constant(_)) | (Constant(_), Variable(t1))  -> 
          let t1_reg = get_reg_from_variable t1 in 
          let reg_result = if peek_write_reg() = _NONE then get_next_register() else write_to_reg() in
          evaluate_ops ops t1_reg _NONE (*second parameter not used*) reg_result
      (* Example y:= 1; z = 0; x := z + y *)
      (* new register is required for sure, we can't overwrite other variables registers *)
      | (Variable(t1), Variable(t2)) -> 
          let t1_reg = get_reg_from_variable t1 in 
          let t2_reg = get_reg_from_variable t2 in 
          let reg_result = if peek_write_reg() = _NONE then get_next_register() else write_to_reg() in
          evaluate_ops ops t1_reg t2_reg reg_result
      (* If we arrive there, then the assignment it's of the form (x = z + 3 + y * z) *)
      (* base case: if the variabile doesn't have a reg, then we assign to it the last one we have used  *)
      (* Not that we can assign it, since for sure the last register is either a new register or a one derived from expressions computation *)
      (* Therefore the last register is for sure not assigned to another variable *)
      | (Variable(t1), other) | (other, Variable(t1)) ->  
          let t1_reg = get_reg_from_variable t1 in 
          (* this call can generate intermediate registers to allocate temporary results *)
          let t2_res = translate_ops other no_reg no_peek in 
          let last = get_last_register () in 
          (* optimize register usage. we reuse last register if we aren't at then end of evalutation *)
          let next_register = if peek_write_reg() = _NONE then last else write_to_reg() in  
          t2_res @ evaluate_ops ops t1_reg last next_register
      (* No variable occur there for sure, otherwise it would have been matched with a previous pattern *)
      (* we can safeily take the last register *)
      (* Optimized register use as we compute at most one loadi and then only addi/multi/subi *)
      | (Constant(_), other) | (other, Constant(_)) ->  
        (* this call can generate intermediate registers to allocate temporary results *)
        let t2_res = translate_ops other no_reg no_peek in 
        let last = get_last_register () in 
        let next_register = if peek_write_reg() = _NONE then last else write_to_reg() in  
        t2_res @ evaluate_ops ops last _NONE next_register
      (* I can resue the last register *)
      (* Remember the last register is either a new register or a register allocated by an expression *)
      (* Either case, the resulting register is for sure not assigned to a variable *)
      | (other, other2) -> 
        let res1 = translate_ops other no_reg no_peek in 
        let last_reg = get_last_register () in 
        let res2 = translate_ops other2 no_reg no_peek in 
        let last_reg2 = get_last_register () in 
        let next_register = if peek_write_reg() = _NONE then last_reg2 else write_to_reg() in  
        res1 @ res2 @ evaluate_ops ops last_reg last_reg2 next_register
      )
  in
  (* All this operations use additional registers to store intermediate results *)
  let rec translate_boolean boolean =
    match boolean with
    | MiniImp.True -> [LoadI(1, get_next_register())]
    | False -> [LoadI(0, get_next_register())]
    | Not(t1) -> 
        let res = translate_boolean t1 in 
        let last = get_last_register() in 
        (* use same register to store the "not" result *)
        res @ [Not(last,last)]
    | And(True, t1) | And(t1, True) ->  
        let true_res = translate_boolean t1 in 
        let last = get_last_register() in  
        (* use same register to store the result *)
        true_res @ [AndI(last,1, last)]
    | And(False, t1) | And(t1, False) ->  
        let false_res = translate_boolean t1 in 
        let last = get_last_register() in  
        (* use same register to store the result *)
        false_res @ [AndI(last, 0, last)]
    | And(t1,t2) -> 
        let t1_res = translate_boolean t1 in 
        let t1_reg = get_last_register() in 
        let t2_res = translate_boolean t2 in 
        let t2_reg = get_last_register() in 
        (* use same register to store the result *)
        t1_res @ t2_res @ [And(t1_reg, t2_reg, t2_reg )]
    (* get a new register, we can't reuse them since they belong to some variables *)
    | Smaller(Variable(t1),Variable(t2)) -> 
      let t1_reg = get_reg_from_variable t1 in 
      let t2_reg = get_reg_from_variable t2  in 
      [Less(t1_reg, t2_reg, get_next_register())]
    (* for sure t2 is not a variable, otherwise we would have matched the case before *)
    (* Therefore I can reuse the same register to store the result (hence t2) *)
    | Smaller(Variable(t1),t2) | Smaller(t2,Variable(t1)) -> 
        let t1_reg = get_reg_from_variable t1 in 
        let t2_res = translate_ops t2 no_reg no_peek in 
        let t2_reg = get_last_register() in   
        t2_res @[Less(t1_reg, t2_reg, t2_reg)]
    (* If we fall in this case, then for sure the expression doesn't contain a variable *)
    (* Therefore I reuse the last register *)
    | Smaller(t1,t2) -> 
      let t1_res = translate_ops t1 no_reg no_peek in 
      let t1_reg = get_last_register() in 
      let t2_res = translate_ops t2 no_reg no_peek in 
      let t2_reg = get_last_register() in   
      t1_res @ t2_res @[Less(t1_reg, t2_reg, t2_reg)]
  in
  let translate_statement statement =
    match statement with
    |MiniImpControlFlow.Skip -> [Nop]
    |Assign(variable,t2) -> 
      (* Assumption #1: If the variable doesn't have a register, then, the first time, we use assign to him the last register used for evaluating its t2  *)
      (* Therefore, to make sure the assumption holds, we allow throwing an exception if some operation tries to read the variable register without peeking it first *)
      (* Lazy functions to postpone evaluating *)
      (* translate_ops either uses a register allocated by an expression or a totally new register if the computation involved only variables *)
      let write_reg () = get_reg_from_variable variable in
      let peek_write_reg () = get_reg_or_empty variable in
      let eval_body = translate_ops t2 write_reg peek_write_reg in
      (* avoid having different registers for same variables *)
      (* link variable to the selected register *)
      (* This assert always holds if intermediate computations haven't broken the assumption *)
      assert (match (get_reg_or_empty variable, get_last_reg_if_none variable) with
        |("",_) -> true
        |(val1,val2) -> val1 = val2 );
      (link_variable variable (get_last_reg_if_none variable);
      eval_body (*@ [LoadI(memory_address (),next_register); Store (get_prec_register (), next_register )]*))
    |Guard(cond) -> 
      let translated_cond = translate_boolean cond in 
      translated_cond
    in
  match node with
  | [] -> []
  |statement :: rest' -> (let res = translate_statement statement in res @ translate_node rest');;
  

let mini_risc_cfg cfg = 
  match cfg with
  | (_nodes, _edges) ->
      let res = ref [] in
      (Hashtbl.iter (fun x _ -> (res :=  x :: !res)) _nodes;
      let sorted = List.sort compare !res in
      let new_nodes = Hashtbl.create 256 in
      let rec get_nodes sorted_nodes =
        match sorted_nodes with
        | [] -> []
        | node:: sorted_nodes' -> let res = translate_node (try Hashtbl.find _nodes node with |_ -> failwith "Porco dio") in ( Hashtbl.add new_nodes node (add_node res); res :: get_nodes sorted_nodes') in
        let _ = get_nodes sorted in
        let rec get_nodes list =
          match list with
          | [] -> []
          | x:: list' -> (Hashtbl.find new_nodes x) :: get_nodes list' in
        (nodes, (Hashtbl.iter (fun x y -> ignore ( add_edge (try (Hashtbl.find new_nodes x) with |_ -> failwith (Printf.sprintf "Not found %s" (string_of_int x))) (get_nodes y)  )) _edges; edges)) 
      );;