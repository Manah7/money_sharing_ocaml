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

let print_path path = List.iter (fun (id1, id2, (flowloc, capa))-> Printf.printf "%d ---(%d/%d)---> %d, " id1 flowloc capa id2) path; Printf.printf "\n"

let drop_zeros gr = e_fold gr (fun tgr id1 id2 (x,y)-> if x = 0 then tgr else new_arc tgr id1 id2 (x,y)) (clone_nodes gr)

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
  | (src, dst, (f,c))::rest -> if c < (flow_min rest) then c else (flow_min rest)


(* Find and return a path between two node. Return None if all path are null *)
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
  |((id1,id2,(f, c))::tail) -> update_capa (add_vsarc ffgr id1 id2 (flow,-flow)) tail flow


let drop_return_arcs gr gri = e_fold gr (fun grf id1 id2 (f,c)->if find_arc gri id1 id2 = None then grf else new_arc grf id1 id2 (f,c)) (clone_nodes gr)

let ford_fulkerson gr src dst =
  let ffgr = init_f_graph gr in
  let rec update_gr ffgr = 
    match find_path ffgr src dst [] with
    | None -> ffgr
    | Some p -> print_path p; Printf.printf "\nFlow min %d\n" (flow_min p); update_gr (update_capa ffgr p (flow_min p))(* CRÉER UNE FONCTION UPDATE_GRAPH PATH *)
  in
  drop_return_arcs (gmap (update_gr ffgr) (fun (c,f)->(c,c+f))) gr

let test_ff gr src dst = let path = find_path (init_f_graph gr) 0 5 [] in
  write_file_path "./outfile_ff" path (path_capa path)


(* **************************************** *)
(* Adding fordfulkerson with float capacity *)
(* **************************************** *)

type vsarc_f = (float * float)

let init_f_graph_f gr = gmap gr (fun x -> (0.0,x))

let rec update_capa_f ffgr path flow = match path with
  | [] -> ffgr
  |((id1,id2,(f, c))::tail) -> update_capa_f (add_vsarc_f ffgr id1 id2 (flow,-.flow)) tail flow

let rec flow_min_f path = match path with
  | [] -> max_float
  | (src, dst, (f,c))::rest -> if c < (flow_min_f rest) then c else (flow_min_f rest)


let rec find_path_f ffgr src dst marked = 
  let arcs_sortants = out_arcs ffgr src in
  let rec explore arc_list = match arc_list with
    | [] -> None
    | (d, (f, c))::_ when dst = d && c > 0.0 -> Some [(src, dst, (f, c))]
    | (id, (f, c))::rest ->
      let path = if (c > 0.0 && not (List.exists (fun x -> x = id) marked)) 
        then find_path_f ffgr id dst (id::marked) 
        else None 
      in
      match path with
      | None -> explore rest
      | Some p -> Some ((src, id, (f, c))::p)
  in
  explore arcs_sortants

let ford_fulkerson_f gr src dst =
  let ffgr = init_f_graph_f gr in
  let rec update_gr ffgr = 
    match find_path_f ffgr src dst [] with
    | None -> ffgr
    | Some p -> update_gr (update_capa_f ffgr p (flow_min_f p))
  in
  drop_return_arcs (gmap (update_gr ffgr) (fun (c,f)->(c,c+.f))) gr

(* ****************** *)
(* Float version test *)
(* ****************** *)

let path_flow_f = function 
  | Some pth ->    List.fold_left (fun x (_,_,(flow,_))-> x +. flow) 0.0 pth
  | None -> -1.0

let path_capa_f = function 
  | Some pth ->    List.fold_left (fun x (_,_,(_,capa))-> x +. capa) 0.0 pth
  | None -> -1.0

let write_file_path_f file_path pth flow = match pth with
  | Some path ->
    let ff = open_out file_path in
    List.iter (fun (id1, id2, (flowloc, capa))-> fprintf ff "%d ---(%.2f/%.2f)---> %d, " id1 flowloc capa id2) path;
    fprintf ff "\n\nFlow total = %.2f\n" flow;
    close_out ff;
    ()
  | None -> 
    let ff = open_out file_path in
    fprintf ff "\nNo path found.\n";
    close_out ff;
    ()

let test_ff_f gr src dst = 
  let path = find_path_f (init_f_graph_f gr) 0 5 [] in
  write_file_path_f "./outfile_ff" path (path_capa_f path)