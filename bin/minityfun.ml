type variable = string [@@deriving show];;


type 't env = variable -> 't [@@deriving show];;

type evt =
|TInt
|TBool
|Arrow of evt * evt[@@deriving show]

and term =
  |Int
  |Bool
  |Variable of variable
  |Fun of variable * evt * term
  |Apply of term * term
  |Plus of term * term
  |Substraction of term * term
  |Times of term * term
  |Minus of term * term
  |Not of term
  |And of term * term
  |IfThenElse of term * term * term
  |LetIn of variable * term * term
  |LetFunIn of variable * variable * evt * term * term [@@deriving show];;



let emptyenv = function x -> failwith (Printf.sprintf "Variable %s not binded" x);;

let bind (s: evt env) (x: variable) (v: evt) = 
  function (i: variable) -> if (i = x) then v else (s i)



let type_check env program = 
  let rec checker env program = 
    match program with
    | Int -> TInt
    | Bool -> TBool
    | Variable(v) -> (env v)
    (* Da rivedere *)
    | Fun(arg, arg_type, body) -> (match arg_type with
      |Arrow(_,_) -> failwith "Type of Fun can not be a function"
      |_ -> Arrow(arg_type,checker (bind env arg arg_type) body)
      )
    | Apply(term1, term2) -> (match (checker env term1) with
      |Arrow(arg_type, return_type) -> (match ((checker env term2) = arg_type) with
        | true -> return_type
        | false -> failwith "Argument type does not match application type"
      )
      | _ -> failwith "Type is not Arrow"
      )
    | Plus(term1, term2) -> (match (checker env term1,checker env term2) with
      |(TInt,TInt) -> TInt
      |_ -> failwith "Plus operator cannot be used with types that are not int"
      )
    | Substraction(term1, term2) -> (match (checker env term1,checker env term2) with
      |(TInt,TInt) -> TInt
      |_ -> failwith "Substraction operator cannot be used with types that are not int"
      )
    | Times(term1, term2) -> (match (checker env term1,checker env term2) with
      |(TInt,TInt) -> TInt
      |_ -> failwith "Times operator cannot be used with types that are not int"
      )
    | And(term1, term2) -> (match (checker  env term1, checker env term2) with
      | (TBool, TBool) -> TBool
      | _ -> failwith "And operator cannot be applied to terms that are not boolean"
      )
    | Minus(term1, term2) -> (match (checker  env term1, checker env term2) with
      | (TInt, TInt) -> TBool
      | _ -> failwith "Minus operator cannot be applied to terms that are not int"
      )
    | Not(term) -> if checker env term = TBool then TBool else failwith "Not operator can not be applied to term that is not a boolean"
    | IfThenElse(condition, if_true, if_false) -> if (checker env condition) = TBool 
      then (let body_type = checker env if_true in
        match (body_type == (checker env if_false)) with 
          |true -> body_type
          |false -> failwith "Branches of if do not have the same type"
          ) 
      else failwith "If condition isn' a bool condition"
    | LetIn(var,body, t2) -> checker (bind env var (checker env body)) t2
    | LetFunIn(fname, arg,ftype, body, t2) -> (match ftype with
      | Arrow(in_type,out_type) -> (
        let fcontext = bind env fname ftype in
        let _context = (bind fcontext  arg in_type) in 
        if (checker _context body) = out_type then checker fcontext t2
        else failwith "Body type does not match the specified returning type")
      | _ -> failwith "Type provided to argument is not an Arrow"
      )
    
  in
  (try Some(checker env program) with
  |Failure (cause) ->  print_endline cause; None
  |_ -> None);;


  let static_program = LetIn(
    "f",
    LetIn(
      "a",
      Int, 
      Fun(
        "y",
        TInt, 
        Plus(
          Variable "y",
          Int
        )
      )
    ),
    LetIn(
      "a",
      Int, 
      Apply(
        Variable "f", 
        Int
      )
    )
  );;

match type_check emptyenv static_program with
| Some(evt) -> print_endline (show_evt evt)
| None -> print_endline "Not a valid program";;


let apply_not_ending_recursive_function = LetFunIn(
  "g",
  "y", 
  Arrow(TInt, TInt),
  Apply(
        Variable "g",
        Substraction(
          Variable "y", 
          Int
        )
      ), 
  Apply(
    Variable("g"),
    Int
  )
);;

match type_check emptyenv apply_not_ending_recursive_function with
| Some(evt) -> print_endline (show_evt evt)
| None -> print_endline "Not a valid program";;


let program_that_fails = LetFunIn("f","x", Arrow(TInt,TInt),Plus(Bool,Variable "y"),Apply(Variable("f"),Int));;
match type_check emptyenv program_that_fails with
| Some(evt) -> print_endline (show_evt evt)
| None -> print_endline "Not a valid program";;