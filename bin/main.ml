let () = print_endline "Hello, World!"

(* Test *)


type state = 
  |State
and 'letter transition = 
  |Transition of 'letter * state * state
and 'letter input = 
  |Input of 'letter list;;

type ('inputs, 'letter) automa = 
  |Automa of state list * 'inputs input * 'letter transition list * state * state list

let state1 = State;;

let state2 = State;;

let state3 = State;;
let state4 = State;;
let state5 = State;;
let state6 = State;;


let input_list = Input(["a";"b";"c";"d";"w";"h";"i";"l";"e"]);;
let transition1 = Transition("w",state1, state2);;

let transition2 = Transition("h",state2, state3);;
let transition3 = Transition("i",state3, state4);;
let transition4 = Transition("l",state4, state5);;
let transition5 = Transition("e",state5, state6);;



let automa = Automa([state1;state2;state3;state4;state5;state6], input_list, [transition1;transition2;transition3;transition4;transition5], state1, [state6] );;

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;


explode "ciao";;
automa;;
(*
let checker (input:string) current_state =
    match explode input with
    | [] -> true
    | letter::rest' ->  *)