open Graph

let clone_nodes gr = n_fold gr new_node empty_graph

(* Take an arc-empty copy and create new arcs with mapped function. (fun . -> .) + fold 
 iterate on all arc *)
let gmap gr f = e_fold gr (fun tgr id1 id2 n -> new_arc tgr id1 id2 (f n)) (clone_nodes gr)

let add_arc gr id1 id2 n = 
    let arc = find_arc gr id1 id2 in
    match arc with
    | None -> new_arc gr id1 id2 n
    | Some a -> new_arc gr id1 id2 (a + n)


let add_vsarc gr id1 id2 (f, c) = 
    let arc = find_arc gr id1 id2 in
    match arc with
    | None -> new_arc gr id1 id2 (f, c)
    | Some (fa, ca) -> 
        let nc = ca + c in
        let nf = fa + f in
        new_arc (new_arc gr id2 id1 (nc, nf)) id1 id2 (nf, nc)


let map_arc gr id1 id2 f =
     let arc = find_arc gr id1 id2 in
    match arc with
    | None -> raise Not_found
    | Some a -> new_arc gr id1 id2 (f a)

