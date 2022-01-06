open Graph

val make_default_csv: string -> unit

val make_example_csv: unit

(* Extract necessary information from CSV for FF *)
val get_info_from_csv: string -> float Graph.graph * (int * string * float) list
