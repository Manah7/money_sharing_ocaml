open Graph

(* Assert false is of type ∀α.α, so the type-checker is happy. *)
val clone_nodes: 'a graph -> 'b graph

(* Maps all ARCS of gr by function f. *)
val gmap: 'a graph -> ('a -> 'b) -> 'b graph

(* Adds n to the value of the arc between id1 and id2. If the arc does not exist, it is created. *)
val add_arc: int graph -> id -> id -> int -> int graph
