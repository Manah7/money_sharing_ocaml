open Graph
open Tools
open Printf

type flow = int

type capa = int

type vsarc = (flow * capa)

type path = (id * id * vsarc) list

type ff_graph = vsarc graph


let write_file_path file_path path flow =
    let ff = open_out file_path in
        List.iter (fun (id1, id2, (flowloc, capa))-> fprintf ff "%d -(%d / %d)-> " id1 flowloc capa) path;
        fprintf ff "\n\nFlow total = %d\n" flow;
        close_out ff;
        ()

(* Take a int graph and return a ff graph *)
let init_f_graph gr = gmap gr (fun x -> (0,x))

(* Return path's flow *)
let path_flow pth = List.fold_left (fun x (_,_,(flow,_))-> x + flow) 0 pth

(* Find and return a path between two node. Return None if all path are null *)
(* TODO : Ajouter une condition d'arrêt et retourner liste node *)
let rec find_path ffgr src dst marked = 
    let arc_list = out_arcs ffgr src in
    let rec explore arc_list = match arc_list with
        | [] -> None
        | (id, (_, c))::rest -> 
            let path = if (c > 0 && not (List.exists (fun x -> x = id) marked)) 
                        then find_path ffgr id dst (id::marked) 
                        else None 
            in
            match path with
                | None -> explore rest
                | _ -> path
    in
    explore arc_list


(* 
    Ford Fulkerson steps:
        Init:
            fl <- Null
        While Exist(Path / flow(Path) != 0) do
            d = min(flow(path.arc[*]))
            for all arc in path do
                fl <- arc(d*sens)

*)

let test_ff gr src dst = assert false

(*
let test_ff gr src dst = let path = find_path (init_f_graph gr) src dst in
    match path with
        | None -> ()
        | Some p -> write_file_path "./outfile_ff" p (path_flow p)
*)

let flow_max gr = assert false
