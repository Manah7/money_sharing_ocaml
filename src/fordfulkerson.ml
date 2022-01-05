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

(* [deprecated] Find and return a path between two node. Return None if all path are null *)
let rec old_find_path ffgr src dst marked = 
  let arcs_sortants = out_arcs ffgr src in
  let rec explore arc_list = match arc_list with
    | [] -> None
    | (d, (f, c))::_ when dst = d && c > 0 -> Some [(src, dst, (f, c))]
    | (id, (f, c))::rest -> 
      let path = if (c > 0 && not (List.exists (fun x -> x = id) marked)) 
        then old_find_path ffgr id dst (id::marked) 
        else None 
      in
      match path with
      | None -> explore rest
      | Some p -> Some ((src, id, (f, c))::p)
  in
  explore arcs_sortants

(* Dijkstra version of find_path() *)
let find_path ffgr src dst negative_threshold  = 
  (* A list of marked nodes - init with src *)
  let marked = [src] in
  (* A queue which contains all unexplored arcs known*)
  let queue = Queue.create () in

  (* We add the first arcs to the queue (Pessimum somnum tuum in terra)
  (We map out arcs to path (adding src), then we filter invalid arcs, then 
  we convert to seq in order to add to queue) *)
  let _ = Queue.add_seq queue (
      List.to_seq (
        List.filter (
          fun (s, d, (f, c)) -> c > negative_threshold && not (List.mem d marked)
        ) (List.map (
          fun (d, t) -> (src, d , t)) (out_arcs ffgr src)
        )
      )
    ) in 

  (* Exploring the first element in loop *)
  let rec loop marked queue = 
  match Queue.take_opt queue with
    | None -> None
    | Some (s, d, (f, c)) when dst = d && c > negative_threshold -> Some [(s, d, (f, c))]
    | Some (s, d, (f, c)) -> 
      (* Discovering the outgoing arcs of the node (and we transform arcs in (potential) path) *)
      let arcs_sortants = List.map (fun (nd, t) -> (d, nd, t)) (out_arcs ffgr d) in
      (* Filtering already marked nodes and unusable arcs *)
      let to_add = List.filter (fun (s, d, (f, c)) -> c > negative_threshold && not (List.mem d marked)) arcs_sortants in
      (* Marking discovered nodes (extracting ids and concat. list) *)
      let n_marked = (List.map (fun (_, d, _) -> d) to_add)@marked in
      (* Adding discovered node to queue *)
      let _ = Queue.add_seq queue (List.to_seq to_add) in 

      (* Loop *)
      let path = loop n_marked queue in

      (* Returning value (We concatenate the raised path with the node, then we go above) *)
      match path with
      | None -> None
      | Some [] -> None
      (* We check if we are the parent, else we pass *)
      | Some ((os, od, (fo, oc))::p) -> if os = d then Some ((s, d, (f, c))::(os, od, (fo, oc))::p) else Some ((os, od, (fo, oc))::p)

  in 

  loop marked queue


(* Remove flow (int)from for each arc in path for ffgr *)
let rec update_capa ffgr path flow = match path with
  | [] -> ffgr
  |((id1,id2,(f, c))::tail) -> update_capa (add_vsarc ffgr id1 id2 (flow,-flow)) tail flow


let drop_return_arcs gr gri = e_fold gr (fun grf id1 id2 (f,c)->if find_arc gri id1 id2 = None then grf else new_arc grf id1 id2 (f,c)) (clone_nodes gr)

let ford_fulkerson gr src dst =
  let ffgr = init_f_graph gr in
  let rec update_gr ffgr = 
    match find_path ffgr src dst 0 with
    | None -> ffgr
    | Some p -> print_path p; Printf.printf "\nFlow min %d\n" (flow_min p); update_gr (update_capa ffgr p (flow_min p))(* CRÉER UNE FONCTION UPDATE_GRAPH PATH *)
  in
  drop_return_arcs (gmap (update_gr ffgr) (fun (c,f)->(c,c+f))) gr

let test_ff gr src dst = let path = find_path (init_f_graph gr) 0 5 0 in
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

(* [deprecated] The new version of find_path () is polymorphic and it is no longer 
   necessary to reimplement this function  *)
let rec old_find_path_f ffgr src dst marked = 
  let arcs_sortants = out_arcs ffgr src in
  let rec explore arc_list = match arc_list with
    | [] -> None
    | (d, (f, c))::_ when dst = d && c > 0.0 -> Some [(src, dst, (f, c))]
    | (id, (f, c))::rest ->
      let path = if (c > 0.0 && not (List.exists (fun x -> x = id) marked)) 
        then old_find_path_f ffgr id dst (id::marked) 
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
    match find_path ffgr src dst 0.0 with
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
  let path = find_path (init_f_graph_f gr) 0 5 0.0 in
  write_file_path_f "./outfile_ff" path (path_capa_f path)