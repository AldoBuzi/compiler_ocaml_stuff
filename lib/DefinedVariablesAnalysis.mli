open CFGDataFlowAnalysis
open MiniImpControlFlow

val undefined_variables_analysis : (label, statement list) Hashtbl.t * (label, label list) Hashtbl.t -> string list * (label, StringSet.t) Hashtbl.t * (label, StringSet.t) Hashtbl.t