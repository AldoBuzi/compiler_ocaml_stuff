open MiniImp

type statement = 
|Skip
|Assign of variable * ops
|Guard of boolean [@@deriving show];;

type node =  statement list  [@@deriving show];;
type edge = (int,int list) Hashtbl.t;;
type graph = (int, node) Hashtbl.t *  edge;;

let nodes = Hashtbl.create 256;;
let edges = Hashtbl.create 256;;


let last_id = ref 0 ;;
let push_node id node = 
  id := !id + 1;
  Hashtbl.add nodes !id node;
  nodes;;
type integers_list = integers list [@@deriving show];;
let push_edge id edge = 
  Hashtbl.replace edges id edge;
  edges;;

let find table id = 
  try Hashtbl.find table id
  with
  |_ -> failwith (Printf.sprintf "ID: %d not found" id);;

(* Utility to get human readable representation of cfg, thus making debugging easier *) 
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
  Hashtbl.iter (fun x y -> Printf.printf "ID= %d: [%s]  ->  %s\n" x (get_node_representation y) (try show_integers_list (Hashtbl.find edges x) with |_ -> "[]")) nodes; print_endline "--------";
  print_endline "----------------------------------- \n EDGES:";
  Hashtbl.iter (fun x y -> Printf.printf "ID= [%d]  ->  %s\n" x (show_integers_list y)) edges; print_endline "--------";
  ;;
  
(* order of nodes and edges is important when building the cfg, once it's built, it doesn't matter anymore *)
let build_cfg ast_program =
  let rec build ast_program = 
  (match ast_program with
  | MiniImp.Skip ->  ignore (push_node last_id [Skip]); ignore (push_edge !last_id []); !last_id
  | Assign(t1,t2) -> ignore (push_node last_id [Assign(t1,t2)]); ignore (push_edge !last_id []); !last_id
  | CommandSeq(c1,c2) -> 
    let first_id = build c1 in
    let c1_last_id = !last_id in
    let first_id2 = build c2 in
    let node1 = find nodes (c1_last_id) in
    let node2 = find nodes (first_id2) in
    (* Remove unnecessary skips *)
    let merged = match (List.rev node1,node2) with
    | (Skip::node1', _) -> List.rev node1' @ node2
    | (_, Skip::node2') ->node1@node2'
    | _ -> node1 @ node2
    in
    let _ = Hashtbl.replace nodes c1_last_id merged in
    let _ = Hashtbl.remove nodes (first_id2) in
    let out_edges = find edges (first_id2) in
    let _ = push_edge c1_last_id out_edges in
    let _ = Hashtbl.remove edges first_id2 in
    first_id
  | IfThenElse(cond, if_true, if_false) -> 
    let _ = push_node last_id [Guard cond] in
    let guard_id = !last_id in
    let true_body = build if_true in
    let false_body = build if_false in
    let _ = push_edge guard_id [true_body; false_body] in
    let _ = push_node last_id [Skip] in
    let _ = push_edge true_body [!last_id] in
    let _ = push_edge false_body [!last_id] in
    guard_id
  | WhileDo(cond, body) -> 
    ignore (push_node last_id [Skip]);
    let root_id = !last_id in
    let _ = push_node last_id [Guard(cond)] in
    let _ = push_edge (!last_id-1) [!last_id] in
    let guard_id = !last_id in
    let body_id = build body in
    let _ = push_edge (!last_id) [guard_id] in
    let _ = push_node last_id [Skip] in
    let _ = push_edge (guard_id) [body_id;!last_id] in
    root_id
    ) in
  ignore (build ast_program);
  (nodes, edges)
;;





