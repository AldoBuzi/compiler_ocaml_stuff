open MiniLang.MiniImp
open MiniLang.MiniImpControlFlow
open MiniLang.CFGDataFlowAnalysis
open MiniLang.LiveVariableAnalysis
open MiniLang.DefinedVariablesAnalysis
open MiniLang.LiveRangeOptimization
open MiniLang.RegisterAllocator
open Lexing
let () =
  let colnum pos =
    (pos.pos_cnum - pos.pos_bol) - 1 in
  let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c in
  if Array.length Sys.argv != 2 then
    failwith "Argument MiniImp-program is needed";
  let in_file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel in_file in
  let input_v = print_endline "Write your input: "; read_line () in  
  let program = (try MiniLang.MiniImpParser.prg MiniLang.MiniImpLexer.read lexbuf 
with MiniLang.MiniImpParser.Error -> raise (Failure ("Parse error at " ^ (pos_string lexbuf.lex_curr_p)))
  ) in
    let (nodes,edges) = (match program with
    | Program(_,_,c) -> build_cfg c) in 
    hr_graph nodes edges; print_endline "--------";
    (* *)
    (* There we will have data flow analysis and its result *)
    let undefined_list = undefined_variables_analysis (nodes, edges) in
    if undefined_list != [] then
      let msg =  "Undefined variables detected: [" ^ (List.fold_left (fun acc x  -> acc ^ x ^ ", " ) "" undefined_list ) ^ "]" in
      failwith msg
    else 
    let (_nodes,_edges) = MiniLang.MiniRISCControlFlowGraph.mini_risc_cfg (nodes,edges) in
    (MiniLang.MiniRISCControlFlowGraph.hr_risc_graph _nodes _edges);

    print_endline "\n-------------------  LIVE ANALYSIS  -------------------------\n";
    let (dv_in, dv_out) = live_analysis (_nodes, _edges) in
    Hashtbl.iter (fun x y -> Printf.printf "ID= [%d]  ->  [%s]\n" x (print_set_as_list y)) dv_in; print_endline "--------";
    print_endline "DV_OUT: \n";
    Hashtbl.iter (fun x y -> Printf.printf "ID= [%d]  ->  [%s]\n" x (print_set_as_list y)) dv_out; print_endline "--------";
    let (new_nodes, new_edges) = (live_range_optimization (dv_in, dv_out) (_nodes, _edges)) in
    (MiniLang.MiniRISCControlFlowGraph.hr_risc_graph new_nodes new_edges);
    let (new_nodes, new_edges) = (register_allocator 10 (new_nodes, new_edges) ) in
    (MiniLang.MiniRISCControlFlowGraph.hr_risc_graph new_nodes new_edges);
    (* *)
    MiniLang.MiniRISC.hr_risc (MiniLang.MiniRISC.get_mini_risc (_nodes, _edges));
    print_endline ( Printf.sprintf "%d" (eval program (int_of_string input_v)));
    print_newline()

