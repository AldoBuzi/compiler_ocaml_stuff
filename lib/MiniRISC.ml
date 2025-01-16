type label = string [@@deriving show];;
type register = label [@@deriving show];;

type comm = 
|Nop
|Add of  register * register * register
|Sub of  register * register * register
|Mult of  register * register * register
|And of  register * register * register
|Less of  register * register * register
|AddI of  register * int * register
|SubI of  register * int * register
|MultI of  register * int * register
|AndI of  register * int * register
|Not of register * register
|Copy of register * register
|Load of register * register
|LoadI of int * register
(* value * memory address *)
|Store of register * register
|Jump of label
|CJump of register * label * label [@@deriving show];;


let risc = Hashtbl.create 256;;

let get_label label = 
  match label with
  | 1 -> "main"
  | value -> Printf.sprintf "l%d" value;;

(* returns hashtable where keys are labels and values are the risc instructions *)
let get_mini_risc cfg =
  match cfg with
  | (nodes,edges) -> 
    let get_jump_from_risc instruction l1 l2 = 
      match instruction  with
      | Less(_,_,r3) | And(_,_,r3) -> CJump(r3, l1, l2)
      | Not(_,r2) -> CJump(r2, l1, l2)
      | _ -> failwith ("get_jump_from_risc: this case should have not happened")  in 
    let add_jump_to_node node_id = (match Hashtbl.find edges node_id with
    | [] -> Hashtbl.find nodes node_id
    | elem :: [] -> let node = Hashtbl.find nodes node_id in node @ [Jump(get_label elem)]
    | elem' :: elem'' :: [] -> let node =  Hashtbl.find nodes node_id in node @ [get_jump_from_risc (List.hd (List.rev node)) (get_label  elem') (get_label elem'')]
    | _ -> failwith "add_jump_to_node: Error, we it's impossible to have three branches") in
    let list = List.sort compare (Hashtbl.fold (fun k _ acc -> k :: acc) nodes []) in
    let rec build list =  
      (match list with
      |[] -> []
      | (node:int)::nodes' -> let jnode = add_jump_to_node node in (Hashtbl.add risc node jnode; build nodes')) in
    ignore (build list);
    risc;;

(* Print utility *)
let hr_risc risc_code = 
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
    | CJump(r,l1,l2) -> Printf.sprintf "\tcjump %s %s %s" r l1 l2
    | Jump(l1) -> Printf.sprintf "\tjump %s" l1
    | Store(r1,r2) -> Printf.sprintf "\tstore %s => %s" r1 r2
    in
  let rec get_node_representation node = 
    match node with
    | [] -> ""
    | elem::remaining' -> Printf.sprintf "%s\n%s" (translate_risc elem) (get_node_representation remaining') in
  let sorted = List.sort compare (Hashtbl.fold (fun k _ acc -> k :: acc) risc_code []) in
  let rec print_risc sorted =
    match sorted with
    | [] -> ""
    | elem :: sorted' -> let res = get_node_representation (Hashtbl.find risc_code elem) in Printf.sprintf "%s:%s\n%s" (get_label elem) res (print_risc sorted') in
  print_risc sorted
  ;;

