    type integers
    type variable
    type ops
    type boolean
    type command
    type program
    type 't env = variable -> 't
    
    val emptyenv: 'a -> integers
    val bind: 't env -> variable -> integers -> variable -> integers
    val eval_ops : (variable -> integers) -> ops -> int
    val eval_bool : (variable -> integers) -> boolean -> boolean
    val eval_command: 't env -> command -> 't env
    val eval: program -> integers -> integers
