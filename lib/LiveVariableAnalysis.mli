open CFGDataFlowAnalysis
open MiniRISC


val live_analysis: (int, comm list) Hashtbl.t * (int, int list) Hashtbl.t ->
    (int, StringSet.t) Hashtbl.t * (int, StringSet.t) Hashtbl.t