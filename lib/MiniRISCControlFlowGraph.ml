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
    | LoadI(i,r2) -> if i > 1000 then Printf.sprintf "\tloadi %X => %s" i r2  else Printf.sprintf "\tloadi %d => %s" i r2
    | Store(r1,r2) ->Printf.sprintf "\tstore %s => %s" r1 r2
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

(* Utlities *)
let _NONE = "";;
let no_reg = fun () -> _NONE;;
let no_peek = no_reg;;

(* END Utilitis *)

let rec translate_node node = 
  let rec translate_ops ops write_reg = 
    match ops with
    (* Example x := 0 *)
    | MiniImp.Constant(t1) -> 
        [LoadI(t1, write_reg ())]
    (* Example y:= 1; x := y *)
    | Variable(t1) -> 
      let t1_reg = get_reg_from_variable t1 in 
      [Copy(t1_reg, write_reg() )]
    | Plus(t1,t2) | Minus(t1,t2) | Times(t1,t2) -> (match (t1,t2) with
      (* Example x := 1 + 2 *)
      (* This will generate (loadi and addi) or (loadi and multi), etc. *)
      | (Constant(_), Constant(_)) -> 
          let r1 = get_next_register () in
          evaluate_ops ops r1 _NONE (* second parameter not required *)  (write_reg())
      (* Example y:= 1; x := y + 1 *)
      (* this kind of condition will generate an addi, multi etc. for sure *)
      | (Variable(t1), Constant(_)) | (Constant(_), Variable(t1))  -> 
          let t1_reg = get_reg_from_variable t1 in 
          evaluate_ops ops t1_reg _NONE (*second parameter not used*) (write_reg())
      (* Example y:= 1; z = 0; x := z + y *)
      | (Variable(t1), Variable(t2)) -> 
          let t1_reg = get_reg_from_variable t1 in 
          let t2_reg = get_reg_from_variable t2 in 
          evaluate_ops ops t1_reg t2_reg (write_reg())
      (* If we arrive there, then the assignment is of the form (x = z + 3 + y * z) *)
      | (Variable(t1), other) | (other, Variable(t1)) ->  
          let t1_reg = get_reg_from_variable t1 in 
          (* this call can generate intermediate registers to allocate temporary results *)
          let t2_res = translate_ops other get_next_register in 
          let last = get_last_register () in     
          t2_res @ evaluate_ops ops t1_reg last (write_reg())
      (* No variable occur there for sure, otherwise it would have been matched with a previous pattern *)
      | (Constant(_), other) | (other, Constant(_)) ->  
        (* this call can generate intermediate registers to allocate temporary results *)
        let t2_res = translate_ops other get_next_register in 
        let last = get_last_register () in   
        t2_res @ evaluate_ops ops last _NONE (write_reg())            
      | (other, other2) -> 
        let res1 = translate_ops other get_next_register in 
        let last_reg = get_last_register () in 
        let res2 = translate_ops other2 get_next_register in 
        let last_reg2 = get_last_register () in         
        res1 @ res2 @ evaluate_ops ops last_reg last_reg2 (write_reg())
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
        res @ [Not(last,get_next_register())]
    | And(True, t1) | And(t1, True) ->  
        let true_res = translate_boolean t1 in 
        let last = get_last_register() in        
        true_res @ [AndI(last,1, get_next_register())]
    | And(False, t1) | And(t1, False) ->  
        let false_res = translate_boolean t1 in 
        let last = get_last_register() in  
        false_res @ [AndI(last, 0, get_next_register())]
    | And(t1,t2) -> 
        let t1_res = translate_boolean t1 in 
        let t1_reg = get_last_register() in 
        let t2_res = translate_boolean t2 in 
        let t2_reg = get_last_register() in 
        t1_res @ t2_res @ [And(t1_reg, t2_reg, get_next_register() )]
    | Smaller(Variable(t1),Variable(t2)) -> 
      let t1_reg = get_reg_from_variable t1 in 
      let t2_reg = get_reg_from_variable t2  in 
      [Less(t1_reg, t2_reg, get_next_register())]
    | Smaller(Variable(t1),t2) | Smaller(t2,Variable(t1)) -> 
        let t1_reg = get_reg_from_variable t1 in 
        let t2_res = translate_ops t2 get_next_register in 
        let t2_reg = get_last_register() in   
        t2_res @[Less(t1_reg, t2_reg, get_next_register())]
    | Smaller(t1,t2) -> 
      let t1_res = translate_ops t1 get_next_register in 
      let t1_reg = get_last_register() in 
      let t2_res = translate_ops t2 get_next_register in 
      let t2_reg = get_last_register() in   
      t1_res @ t2_res @[Less(t1_reg, t2_reg, get_next_register())]
  in
  let translate_statement statement =
    match statement with
    |MiniImpControlFlow.Skip -> [Nop]
    |Assign(variable,t2) -> 
      (* Lazy functions to postpone evaluating *)
      let write_reg () = get_next_reg_if_none variable in
      let peek_write_reg () = get_reg_or_empty variable in
      let eval_body = translate_ops t2 write_reg in
      (* link variable to the selected register *)
      link_variable variable (get_last_reg_if_none variable);
      eval_body
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
    let res = Hashtbl.fold (fun x _ acc -> x :: acc ) _nodes [] in
    let sorted = List.sort compare res in
    let new_nodes = Hashtbl.create 256 in
    let rec get_nodes sorted_nodes =
      match sorted_nodes with
      | [] -> []
      | node:: sorted_nodes' -> let res = translate_node (try Hashtbl.find _nodes node with |_ -> failwith "TODO write exception") in ( Hashtbl.add new_nodes node (add_node res); res :: get_nodes sorted_nodes') in
      let _ = get_nodes sorted in
      let rec get_nodes list =
        match list with
        | [] -> []
        | x:: list' -> (Hashtbl.find new_nodes x) :: get_nodes list' in
      (nodes, (Hashtbl.iter (fun x y -> ignore ( add_edge (try (Hashtbl.find new_nodes x) with |_ -> failwith (Printf.sprintf "Not found %s" (string_of_int x))) (get_nodes y)  )) _edges; edges)) 
;;