open Graph

let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = e_fold gr (fun tgr id1 id2 n -> new_arc tgr id1 id2 (f n)) (clone_nodes gr)

let add_arc gr id1 id2 n = assert false; 


(*
let gmap gr f = e_fold 
let add_arc gr id1 id2 n = match gr with
    |[]-> []
    |(id1,arcs)::tail-> (match arcs with
        |[]->[]
        |id2::tail2 ->  
        |y::tail2->)
    |(x,arcs)::tail->

*)

