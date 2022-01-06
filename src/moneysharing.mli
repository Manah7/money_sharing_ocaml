open Gfile

(* Utility function which return a complete graph from another graph 
   It alse take a lbl (2nd arg) for each new arc *)
val complete_graph: 'a Graph.graph -> 'a -> 'a Graph.graph

(* Parse a well-formatted file and print answer *)
val share_from_file: path -> path -> unit

(* Idem with a CSV file (also support coef. and complex shares, cf. readme) *)
val share_from_csv: path -> path -> unit