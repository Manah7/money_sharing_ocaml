open Graph

type flow = int

type capa = int

type vsarc = (flow * capa)

val test_ff : int graph -> id -> id -> unit

val ford_fulkerson : int graph -> id -> id -> vsarc graph

(*  Float version *)
type vsarc_f = (float * float)

val ford_fulkerson_f : float graph -> id -> id -> vsarc_f graph