open CFGDataFlowAnalysis
open MiniImpControlFlow



let reversed_edges = Hashtbl.create 256;;
let reverse_edges edges = 
  let rec iter_list dest = function
  |[] -> reversed_edges
  | x:: elem' -> Hashtbl.add reversed_edges x dest; iter_list dest elem' in
  Hashtbl.iter (fun key value -> ignore (iter_list key value) ) edges ;;



let initial_variables = ref StringSet.empty;;


let rec init_variables_set blocks nodes = 
  let add_variables = function
  | Assign(variable, _)-> 
    initial_variables := StringSet.add variable !initial_variables;
  | _ -> ignore ()
  in 
  let rec iterate_block b = 
    match b with
    | [] -> []
    | instruction :: b' -> add_variables instruction; iterate_block b'
    in
  (* Special variable added statically *)
  initial_variables := StringSet.add "in" !initial_variables;
  match blocks with
  | [] -> ()
  | x :: lis' -> ignore (iterate_block (Hashtbl.find nodes x)); init_variables_set lis' nodes


let rec get_defined_variables block = 
  match block with
  | [] -> StringSet.empty
  | ins :: block' -> (match ins with
    | Assign(variable,_) -> 
      StringSet.add variable (get_defined_variables block')
    | _ -> get_defined_variables block'
  )
;;
(*
 Start of analysis
*)

let defined_variables_analysis (cfg : (label, statement list) Hashtbl.t * (label, label list) Hashtbl.t ) = 
  match cfg with
  |(nodes, edges) ->
    (
      let blocks = List.sort compare (keys_of_hashtable nodes) in 
      init_variables_set blocks nodes;

      (* build hash table of reversed direction of the edges *)
      reverse_edges edges;    
      let l_dv_out block = 
        (*
          DV_OUT STUFF
        *)
        Hashtbl.replace out_regs block (StringSet.union (dv_in block) (get_defined_variables (Hashtbl.find nodes block) ));
      in
      let l_dv_in block = 
        let predecessors = Hashtbl.find_all reversed_edges block in
        (match predecessors with
          (* if empty is initial block*)
        | [] -> Hashtbl.replace in_regs block (StringSet.add "in" StringSet.empty)
        | x:: pred' -> 
          let res_set = find_or_empty_set out_regs x in
          let rec scan_preds set = function
            |[] -> set
            |pred :: pred1' -> scan_preds (StringSet.inter set (dv_out pred)) pred1' in
            Hashtbl.replace in_regs block ( scan_preds res_set pred');
        )
      in
      CFGDataFlowAnalysis.df_analysis blocks !initial_variables l_dv_in l_dv_out
    )
;;

let undefined_variables_analysis = function
  |(nodes, edges) -> 
    let (dv_in, dv_out) = defined_variables_analysis (nodes, edges) in
    let rec get_ops = function
      |MiniImp.Variable(i) -> StringSet.add i StringSet.empty
      |Constant(_) -> StringSet.empty
      |Plus(t1,t2) -> StringSet.union (get_ops t1)  (get_ops t2)
      |Times(t1,t2) -> StringSet.union (get_ops t1) (get_ops t2)
      |Minus(t1,t2) -> StringSet.union (get_ops t1) (get_ops t2) in
    let rec get_boolean = function
      |MiniImp.True -> StringSet.empty
      |False -> StringSet.empty
      |And(t1,t2) ->  StringSet.union  (get_boolean t1) (get_boolean t2)
      |Smaller(t1,t2) ->  StringSet.union  (get_ops t1) (get_ops t2)
      |Not(t1) ->  (get_boolean t1) in
    let rec check_block block in_set =
      match block with
      | [] -> StringSet.empty
      | ins :: block' -> (
        match ins with
          | Assign(variable,ops) -> 
            let expr = get_ops ops in
            let new_undefined = 
              StringSet.fold (fun x acc -> 
                (if x = "in" || StringSet.exists (fun y -> y = x) in_set 
                  then acc
                  else StringSet.add x acc
                )
              ) expr StringSet.empty
            in
            StringSet.union (if StringSet.is_empty new_undefined = false
              then new_undefined
              else StringSet.empty
            ) (check_block block' (StringSet.add variable in_set))
          | Guard(boolean) -> 
            StringSet.fold (fun x acc -> 
              (if x = "in" || StringSet.exists (fun y -> y = x) in_set 
                then acc
                else StringSet.add x acc
              )
            ) (get_boolean boolean) StringSet.empty
          | _ -> check_block block' in_set
        ) 
    in
    (* returns list containing all the undefined variables in the program *)
    (Hashtbl.fold (fun node_id block undefined_set-> 
        let defined_set = Hashtbl.find dv_in node_id in
        let u_set = check_block block defined_set
        in
        (StringSet.fold (fun x  acc -> if (List.exists (fun y -> y = x) undefined_set) then acc else x::acc  ) u_set []) @ undefined_set
       ) nodes [], 
       dv_in,
       dv_out
    )