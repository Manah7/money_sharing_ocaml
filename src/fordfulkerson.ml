open Graph

type path = int out_arcs list

let find_path gr = assert false

(* 
    Ford Fulkerson steps:
        Init:
            fl <- Null
        While Exist(Path / flow(Path) != 0) do
            d = min(flow(path.arc[*]))
            for all arc in path do
                fl <- arc(d*sens)

*)

let flow_max gr = assert false
