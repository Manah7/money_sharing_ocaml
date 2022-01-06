open Graph

val make_default_csv: string -> unit

val make_example_csv: unit

(* Utility function which return a complete graph from another graph 
   It alse take a lbl (2nd arg) for each new arc *)
val complete_graph: 'a Graph.graph -> 'a -> 'a Graph.graph

(* Extract necessary information from CSV for FF *)
val get_info_from_csv: string -> float Graph.graph * (int * string * float) list
