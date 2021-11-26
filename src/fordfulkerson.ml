open Graph
open Tools
open Printf

type flow = int

type capa = int

type vsarc = (flow * capa)

type path = (id * id * vsarc) list

type ff_graph = vsarc graph


let graphe_ecart gr = gmap gr (fun (flow, capa) -> capa-flow)

let write_file_path file_path pth flow = match pth with
    | Some path ->
        let ff = open_out file_path in
            List.iter (fun (id1, id2, (flowloc, capa))-> fprintf ff "%d ---(%d/%d)---> %d, " id1 flowloc capa id2) path;
            fprintf ff "\n\nFlow total = %d\n" flow;
            close_out ff;
            ()
    | None -> 
        let ff = open_out file_path in
            fprintf ff "\nNo path found.\n";
            close_out ff;
            ()

(* Take a int graph and return a ff graph *)
let init_f_graph gr = gmap gr (fun x -> (0,x))

(* Return path's flow *)
let path_flow = function 
    | Some pth ->    List.fold_left (fun x (_,_,(flow,_))-> x + flow) 0 pth
    | None -> -1

let path_capa = function 
    | Some pth ->    List.fold_left (fun x (_,_,(_,capa))-> x + capa) 0 pth
    | None -> -1

let rec flow_min path = match path with
    | [] -> max_int
    | (src, dst, (f,c))::rest -> if f < (flow_min rest) then f else (flow_min rest)


(* Find and return a path between two node. Return None if all path are null *)
(* TODO : Ajouter une condition d'arrêt et retourner liste node *)
let rec find_path ffgr src dst marked = 
    let arcs_sortants = out_arcs ffgr src in
    let rec explore arc_list = match arc_list with
        | [] -> None
        | (d, (f, c))::_ when dst = d && c > 0 -> Some [(src, dst, (f, c))]
        | (id, (f, c))::rest -> 
            let path = if (c > 0 && not (List.exists (fun x -> x = id) marked)) 
                        then find_path ffgr id dst (id::marked) 
                        else None 
            in
            match path with
                | None -> explore rest
                | Some p -> Some ((src, id, (f, c))::p)
    in
    explore arcs_sortants


(* Remove flow (int)from for each arc in path for ffgr *)
let rec update_capa ffgr path flow = match path with
    | [] -> ffgr
    |((id1,id2,_)::tail) -> update_capa (map_arc ffgr id1 id2 (fun (f,c) -> (f+flow,c-flow)))  tail flow

(* 
    Ford Fulkerson steps:
        Init:
            fl <- Null
        While Exist(Path / flow(Path) != 0) do
            d = min(flow(path.arc[*]))
            for all arc in path do
                fl <- arc(d*sens)
*)



let ford_fulkerson gr src dst =
    let ffgr = init_f_graph gr in
    let rec update_gr ffgr = match find_path ffgr src dst [] with
        | None -> ffgr
        | Some p -> update_gr (update_capa ffgr p (flow_min p))(* CRÉER UNE FONCTION UPDATE_GRAPH PATH *)
    in
    update_gr ffgr


let test_ff gr src dst = let path = find_path (init_f_graph gr) 0 5 [] in
    write_file_path "./outfile_ff" path (path_capa path)


let flow_max gr = assert false
