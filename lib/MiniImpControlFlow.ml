open MiniImp
open GenericCFG
type statement = 
|Skip
|Assign of variable * ops
|Guard of boolean [@@deriving show];;
open GenericCFGImpl(struct type node = statement list end)


(* Utility to get human readable representation of cfg, this way I make debugging easier *) 
let hr_graph nodes edges = 
  let rec get_string_a = function
   |Variable(i) -> i
   |Constant(i) -> string_of_int(i)
   |Plus(t1,t2) -> Printf.sprintf "%s + %s" (get_string_a t1) (get_string_a t2)
   |Times(t1,t2) -> Printf.sprintf "%s * %s" (get_string_a t1) (get_string_a t2)
   |Minus(t1,t2) -> Printf.sprintf "%s - %s" (get_string_a t1) (get_string_a t2) in
  let rec get_string_b = function
  |True -> "true"
  |False -> "false"
  |And(t1,t2) -> Printf.sprintf "%s && %s" (get_string_b t1) (get_string_b t2)
  |Smaller(t1,t2) -> Printf.sprintf "%s < %s" (get_string_a t1) (get_string_a t2)
  |Not(t1) -> Printf.sprintf "not %s" (get_string_b t1) in
  let rec get_node_representation node = 
    match node with
    | [] -> ""
    | Skip::remaining' -> Printf.sprintf "Skip; %s" (get_node_representation remaining')
    | Assign(t,a)::remaining' -> Printf.sprintf "%s := %s; %s" t (get_string_a a) (get_node_representation remaining')
    | Guard(cond)::remaining' -> Printf.sprintf  "%s?; %s" (get_string_b cond) (get_node_representation remaining') in
  Hashtbl.iter (fun x y -> Printf.printf "ID= %d: [%s]  ->  [%s]\n" x (get_node_representation y) (try show_label_list (Hashtbl.find edges x) with |_ -> "[]")) nodes; print_endline "--------";
  print_endline "----------------------------------- \n EDGES:";
  Hashtbl.iter (fun x y -> Printf.printf "ID= %d  ->  [%s]\n" x (show_label_list y)) edges; print_endline "--------";
  ;;
  
let build_cfg ast_program =
  (* The tuple represents the "i1" and "f1" of the cfg representation *)
  let rec build ast_program = 
    (* TODO : Fix while skips*)
  (match ast_program with
  | MiniImp.Skip ->  let id = add_node [Skip] in ignore (add_edge (id) []); (id,id)


  | Assign(t1,t2) -> let id =  add_node [Assign(t1,t2)] in ignore (add_edge (id) []); (id,id)


  | CommandSeq(c1,c2) -> 
    let (c1_start,c1_end) = build c1 in
    let (c2_start,c2_end) = build c2 in
    let node1 = find nodes c1_end in
    let node2 = find nodes c2_start in
    (* Remove unnecessary skips *)
    let merged = match (List.rev node1,node2) with
    | (Skip::node1', _) -> List.rev node1' @ node2  
    | _ -> node1 @ node2 
    in
    Hashtbl.replace nodes c1_end merged;
    Hashtbl.remove nodes c2_start;
    let out_edges = find edges c2_start in
    let in_edge = try find reversed_edges c2_start with | _ -> -1 in
    ignore (add_edge c1_end out_edges);
    if in_edge != -1 then ignore (add_edge in_edge [c1_end]);
    Hashtbl.remove edges c2_start;
    (c1_start, c2_end)


  | IfThenElse(cond, if_true, if_false) -> 
    let guard_id = add_node [Guard cond] in
    let (t_body_start, t_body_end) = build if_true in
    let (f_body_start,f_body_end) = build if_false in
    ignore (add_edge guard_id [t_body_start; f_body_start]);
    let skip_id = add_node [Skip] in
    ignore (add_edge t_body_end [skip_id]);
    ignore (add_edge f_body_end [skip_id]);
    ignore (add_edge skip_id []);
    (guard_id,skip_id)


  | WhileDo(cond, body) -> 
    let guard_id = add_node [Guard(cond)] in
    let (body_start,body_end) = build body in
    let skip_id = add_node [Skip] in 
    ignore (add_edge (guard_id) [body_start; skip_id]);
    let b = Hashtbl.find_opt nodes body_end in
    ignore (add_edge (if b = None then body_start else body_end) [guard_id]);
    ignore (add_edge skip_id []);
    (guard_id, skip_id)
    ) 
  in
  let (istart, _) = build ast_program in
  (*if it has outgoing edges, than for sure it was a incoming edges*)
  (match (try find reversed_edges istart with |_ -> -1  ) with
  | value -> (
    if value != -1 then 
    (Hashtbl.replace nodes 0 [Skip];
    ignore (add_edge 0 [istart]);
    )
  ));
  (nodes, edges)
;;





