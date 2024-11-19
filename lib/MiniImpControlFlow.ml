open MiniImp


type variable = string [@@deriving show];;

type a = 
  |Variable of variable
  |Int of int
  |Plus of a * a
  |Minus of a * a
  |Times of a * a [@@deriving show];;

type b = 
|True
|False
|And of b * b
|Not of b
|Smaller of a * a [@@deriving show];;
type statement = 
|Skip
|Assign of variable * a
|Condition of b [@@deriving show];;

type node = statement list  [@@deriving show];;
type edge = node * node  [@@deriving show];;
type graph = node list * edge list  [@@deriving show];;
let rec next node edge_list =
  match edge_list with
  | [] -> None
  | (_node1, _node2) :: edge_list' -> if _node1 = node then Some _node2 else next node edge_list';;


(* Map MiniImp types to statement, a and b *)
let translate ast = 
  let rec translate_a ast = 
    match ast with
    | MiniImp.Variable(i) -> Variable(i)
    | Constant(i) -> Int( i)
    | Plus(t1,t2) -> Plus(translate_a t1, translate_a t2)
    | Times(t1,t2) -> Times(translate_a t1, translate_a t2)
    | Minus(t1,t2) -> Minus(translate_a t1, translate_a t2) in
  let rec translate_b ast = 
      match ast with
      | MiniImp.True -> True
      | False -> False
      | And(t1,t2) -> And(translate_b t1, translate_b t2)
      | Not(t1) -> Not(translate_b t1)
      | Smaller(t1,t2) -> Smaller(translate_a t1, translate_a t2) in
  match ast with
  | MiniImp.Assign(t1,t2) -> Assign(t1, translate_a t2)
  | Skip -> Skip
  | IfThenElse(cond, _, _) -> Condition(translate_b cond)
  | WhileDo(cond, _) -> Condition(translate_b cond)
  | CommandSeq(_,_) -> failwith "This case must not be matched";;


let merge_edges edges edges2 merged = 
  match (List.rev  edges, edges2) with
  (* Example : C1;Skip *)
  | ((start,_)::edges', []) -> (List.rev edges') @ [(start, merged)]
  (* Example : Skip;IfThenElse*)
  | ([], ([Condition(_)],end')::([Condition(_)],end'')::edges2') -> 
    (merged,   end') :: (merged, end'') :: edges2'
  (* Example : Skip;C2 *)
  | ([], (_,end')::edges2') -> (merged, end') :: edges2'
  (* Example : C1;IfThenElse*)
  | ((start,_)::edges', ([Condition(_)],end')::([Condition(_)],end'')::edges2') -> 
    (List.rev edges') @ ((start, merged) :: (merged,   end') :: (merged, end'') :: edges2')
  (* Example : C1;C2 *)
  | ((start,_)::edges', (_,end')::edges2') -> 
    (List.rev edges') @ (start, merged) :: (merged, end') :: edges2'
  (* Example : Skip;Skip *)
  | ([],[]) -> [];;


(* Utility to get human readable representation of cfg, thus making debugging easier *) 
let rec hr_graph edges = 
  let rec get_string_a = function
   |Variable(i) -> i
   |Int(i) -> string_of_int(i)
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
    | Condition(cond)::remaining' -> Printf.sprintf  "%s?; %s" (get_string_b cond) (get_node_representation remaining') in
  match edges with
  |[] -> ""
  |(start,_end)::edges' -> Printf.sprintf "[%s]  ->   [%s] \n\n%s" (get_node_representation start) (get_node_representation _end) (hr_graph edges');;
  ;;

(* order of nodes and edges is important when building the cfg, once it's built, it doesn't matter anymore *)
let rec build_cfg ast_program =
    match ast_program with
    | MiniImp.Skip -> ([[Skip]],[])
    | Assign(_,_) -> ([[translate ast_program]],[])
    | CommandSeq(c1,c2) -> (
      match (build_cfg c1, build_cfg c2) with 
        |((block1, edges),(block2, edges2)) -> 
            let last_b1 = List.rev block1 in 
            let merged = List.hd last_b1 @ List.hd block2 in
            ( List.rev(List.tl last_b1 ) @ merged :: List.tl block2, merge_edges edges edges2 merged)
      )
    | IfThenElse(_, if_true, if_false) -> 
      let node = [translate ast_program] in 
        (match (build_cfg if_true, build_cfg if_false) with
        | ((block1, edges),(block2, edges2)) -> 
          (
            node::block1@block2@[[Skip]], 
            (node, List.hd block1 ) :: (node, List.hd block2 ) :: (List.hd (List.rev block1), [Skip] ) :: edges @ edges2 @ [(List.hd (List.rev block2), [Skip])]
          )
        )
    | WhileDo(_, body) -> (match build_cfg body with
     | (block1, edges) -> let node = [translate ast_program] in
        (
          [Skip]::node:: block1@[[Skip]], 
          ([Skip], node)::(node,List.hd block1)::(List.hd (List.rev block1),node)::(node,[Skip])::edges
        )
    );;

