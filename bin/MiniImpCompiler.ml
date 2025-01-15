open MiniLang.MiniImp
open MiniLang.MiniImpControlFlow
open MiniLang.LiveVariableAnalysis
open MiniLang.DefinedVariablesAnalysis
open MiniLang.LiveRangeOptimization
open MiniLang.CFGDataFlowAnalysis
open MiniLang.RegisterAllocator
open Lexing

let print_help() = print_endline
"
Usage: dune exec MiniImpCompiler <num_registers> <input_file> <output_path> -- [OPTIONS]

Positional Arguments:
  <num_registers>    Number of registers available in the target machine.
  <input_file>       Path to the MiniImp source file to compile.
  <output_path>      Path to save the translated output file.

Options:
  -h                 Shows this message
  -d                 Enable debug mode to print detailed execution information.
  -op                Enable live range optimization during compilation.
  -uc                Enable undefined variable checking to ensure correctness.

Examples:
  MiniImpCompiler 8 source.mimp output.txt
  MiniImpCompiler 4 source.mimp output_optimized.txt -- -d -op
  MiniImpCompiler 5 input.mimp translated.txt -- -uc

Notes:
  - The <num_registers> argument must be a positive integer.
  - The <input_file> must point to a valid MiniImp source file.
  - The <output_path> must specify a writable location.
  - If no options are provided, the compiler runs with default settings where optimizations and checks are turned off.
";;
let write_to_file filename content =
  (* Open file, if it doesn't exist, then I create it *)
  let oc = open_out filename in
  (* write content *)
  output_string oc content;
  (* close file *)
  close_out oc;;

let () =
  let colnum pos =
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) in
  let pos_string pos=
  let l = string_of_int pos.pos_lnum
  and c = string_of_int (colnum pos) in
    "lines " ^ l ^ ", column " ^ c in
  (*
    Read command line arguments
  *)
  let (registers_target, in_file, output_path, optimization_flag, undefined_variable_flag, debug_flag)  = match Array.length Sys.argv with
    | x when x = 2 -> print_help(); exit 0;
    | x when x < 4  -> failwith "At least, the number of registers the target machine has, the MiniImp-program and output path must be provided";
    | x when x = 4 -> (int_of_string Sys.argv.(1), Sys.argv.(2),Sys.argv.(3), false, false , false)
    | x when x = 5 -> 
      let optimization_flag = Sys.argv.(4) = "-op" in
      let undefined_variable_flag = Sys.argv.(4) = "-uc" in
      let debug_flag = Sys.argv.(4) = "-d" in
      (int_of_string Sys.argv.(1), Sys.argv.(2),Sys.argv.(3), optimization_flag,undefined_variable_flag, debug_flag)
    | x when x = 6 -> 
      let optimization_flag = Sys.argv.(4) = "-op" || Sys.argv.(5) = "-op" in
      let undefined_variable_flag = Sys.argv.(4) = "-uc" || Sys.argv.(5) = "-uc" in
      let debug_flag = Sys.argv.(4) = "-d"  || Sys.argv.(5) = "-d" in
      (int_of_string Sys.argv.(1), Sys.argv.(2),Sys.argv.(3), optimization_flag,undefined_variable_flag, debug_flag)
    | x when x = 7 -> 
      let optimization_flag = Sys.argv.(4) = "-op" || Sys.argv.(5) = "-op"  || Sys.argv.(6) = "-op" in
      let undefined_variable_flag = Sys.argv.(4) = "-uc" || Sys.argv.(5) = "-uc" || Sys.argv.(6) = "-uc" in
      let debug_flag = Sys.argv.(4) = "-d"  || Sys.argv.(5) = "-d" || Sys.argv.(6) = "-d" in
      (int_of_string Sys.argv.(1), Sys.argv.(2),Sys.argv.(3), optimization_flag,undefined_variable_flag, debug_flag)  
    | x when x > 7 -> failwith "At least, the number of registers the target machine has, the MiniImp-program and output path must be provided";  
    | _ -> failwith "MiniImpCompiler - argv parser error"
  in

  (* program input file *)
  let in_file = open_in in_file in
  let lexbuf = Lexing.from_channel in_file in
  let program = (try MiniLang.MiniImpParser.prg MiniLang.MiniImpLexer.read lexbuf 
  with MiniLang.MiniImpParser.Error -> raise (Failure ("Parse error at " ^ (pos_string lexbuf.lex_curr_p)))
  ) in
    let (nodes,edges) = (match program with | Program(_,_,c) -> (*get minimp cfg*) build_cfg c) in 
    if debug_flag then  (
      print_endline "\n----------------------  MINI IMP CFG  ---------------------------\n";
      hr_graph nodes edges 
    );
    
    
    (* There we will have data flow analysis and its result *)
    if undefined_variable_flag then
      (let (undefined_list, dv_in, dv_out) = undefined_variables_analysis (nodes, edges) in
      if debug_flag then 
        (print_endline "\n----------------------  DEFINED VARIABLE ANALYSIS  ---------------------------\n";
        Hashtbl.iter (fun x y -> Printf.printf "ID= [%d]  ->  [%s]\n" x (print_set_as_list y)) dv_in; print_endline "--------";
        print_endline "DV_OUT: \n";
        Hashtbl.iter (fun x y -> Printf.printf "ID= [%d]  ->  [%s]\n" x (print_set_as_list y)) dv_out;);
      (if undefined_list != [] then
        let msg =  "Undefined variables detected: [" ^ (List.fold_left (fun acc x  -> acc ^ x ^ ", " ) "" undefined_list ) ^ "]" in
        failwith msg);)
    else ();
    (* Build MINIRisc CFG *)
    let (_nodes,_edges) = MiniLang.MiniRISCControlFlowGraph.mini_risc_cfg (nodes,edges) in
    if debug_flag then (
      print_endline "\n----------------------  MINI RISC CFG  ---------------------------\n";
      MiniLang.MiniRISCControlFlowGraph.hr_risc_graph _nodes _edges
    );


    let (new_nodes, new_edges) = 
      if optimization_flag then
        let (dv_in, dv_out) = live_analysis (_nodes, _edges) in
        if debug_flag then 
          (print_endline "\n----------------------  LIVE ANALYSIS  ---------------------------\n";
          Hashtbl.iter (fun x y -> Printf.printf "ID= [%d]  ->  [%s]\n" x (print_set_as_list y)) dv_in; print_endline "--------";
          print_endline "DV_OUT: \n";
          Hashtbl.iter (fun x y -> Printf.printf "ID= [%d]  ->  [%s]\n" x (print_set_as_list y)) dv_out;);
        (live_range_optimization (dv_in, dv_out) (_nodes, _edges)) 
      else (_nodes, _edges)
    in
      if debug_flag then  (MiniLang.MiniRISCControlFlowGraph.hr_risc_graph new_nodes new_edges);
    
    (* Register Allocator and mapper of virtual registers to physical *)
    if debug_flag then  (print_endline "\n----------------- REGISTER ALLOCATOR ------------------\n";);
    let (new_nodes, new_edges) = (register_allocator registers_target (new_nodes, new_edges) ) in
    if debug_flag then  (MiniLang.MiniRISCControlFlowGraph.hr_risc_graph new_nodes new_edges );
    
    
    (* save produced code *)
    write_to_file output_path (MiniLang.MiniRISC.hr_risc (MiniLang.MiniRISC.get_mini_risc (_nodes, _edges)));
    let input_v = print_endline "Write your input: "; read_line () in  
    print_endline ( Printf.sprintf "\nThe program output is: %d\n" (eval program (int_of_string input_v)));

