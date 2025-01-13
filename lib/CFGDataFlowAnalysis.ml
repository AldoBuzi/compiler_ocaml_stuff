type label = int ;;


let keys_of_hashtable table =
  Hashtbl.fold (fun key _ acc -> key :: acc) table []
;;

(*
 Stuff to build inital set of registers
*)
module StringSet = Set.Make(struct
  type t = string
  let compare = compare
end);;
let print_set_as_list set =
  let elements = StringSet.elements set in
  String.concat "; " elements
;;


let find_or_empty_set table key = 
  try Hashtbl.find table key with
  | _ -> StringSet.empty;;

let in_regs = Hashtbl.create 256;;
let out_regs = Hashtbl.create 256;;

let dv_in block =
  find_or_empty_set in_regs block;;
  
let dv_out block =
    find_or_empty_set out_regs block;;


let print_set_as_list set =
  let elements = StringSet.elements set in
  String.concat "; " elements
;;

let df_analysis (blocks: label list) initial_set l_dv_in l_dv_out = 
  ( 
      (* init all sets *)
      Hashtbl.clear in_regs;
      Hashtbl.clear out_regs;
      let rec init_dv = function
      |[] -> ()
      | block:: blocks' -> Hashtbl.add in_regs block initial_set; Hashtbl.add out_regs block initial_set; init_dv blocks' in
      init_dv blocks;

      (* compute local dv_in and dv_out *)
      let rec scan_blocks = function
      | [] -> ()
      | block:: blocks' -> 
        (* 
          DV_IN STUFF 
        *)
        l_dv_in block;
        (*
          DV_OUT STUFF
        *)
        l_dv_out block;
        scan_blocks blocks';
        
      in
        let rec find_fix_point g_in g_out =
          if g_in = in_regs && g_out = out_regs then
            (g_in,g_out)
          else 
            let g_in = Hashtbl.copy in_regs in
            let g_out = Hashtbl.copy out_regs in
            scan_blocks blocks;
            find_fix_point g_in g_out 
          in
        find_fix_point (Hashtbl.create 0) (Hashtbl.create 0)
        
  );;