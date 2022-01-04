open Fordfulkerson
open Gfile
open Graph
open Printf
open Tools

let add_user line ul = 
  try Scanf.sscanf line "p %s %f" (fun user amount -> (user, amount)::ul)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "get_graph_from_file"

(* Reads a comment or fail. *)
let read_comment th line =
  try Scanf.sscanf line " %%" th
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "get_graph_from_file"

let rec sum_amount ul = match ul with
  | [] -> 0.0
  | (_, a)::rest -> a +. (sum_amount rest)

(* Add every possible outgoing arc from one given node with label lbl *)
let add_all_arcs_from_node gr id lbl = 
    n_fold gr (fun tgr id2 -> if id != id2 then new_arc tgr id id2 lbl else tgr) gr

(* Make a given graph complete by adding all possible outgoing arcs for every node *)
let complete_graph gr lbl = 
    n_fold gr (fun tgr id -> add_all_arcs_from_node tgr id lbl) gr


(* Based on from_file() (gfile.ml) *)
let rec get_graph_from_file path = 
  let infile = open_in path in

  (* A list of user and amount *)
  let user_list = [] in

  (* Read all lines until end of file, altering graph and user list *)
  let rec loop graph ul =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      (* We add user if there is one *)
      let ul2 =
        if line = "" then ul
        else match line.[0] with
          | 'p' -> add_user line ul
          | _ -> read_comment ul line
      in

      (* We add user to graph *)
      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        else match line.[0] with
          | 'p' -> new_node graph (List.length ul2)
          | _ -> read_comment graph line
      in

      loop graph2 ul2

    with End_of_file -> (graph, ul) (* Done *)
  in


  let data = loop empty_graph user_list in

  (* Adding arcs depending on the total amount*)
  let make_final_graph data = match data with
    | (gr, ul) -> 
      (* Calculation of total amount *)
      let total = sum_amount ul in
      (* Calculation of user part *)
      let user_part = total /. (float_of_int (List.length ul)) in
      (* Adding infinte capacity arc to graph *)
      let fgr = complete_graph gr total in
      (* Add source and sink nodes *)
      let src_id = (List.length ul)+2 in
      let snk_id = (List.length ul)+1 in
      let ffgr = new_node (new_node fgr src_id) snk_id in
      (* Looping on user list to add outward arcs *)
      let rec ul_loop gr ul n = match ul with
        | [] -> gr
        | (name, a)::rest -> 
            let diff = a -. user_part in
            if diff > 0.0 then ul_loop (new_arc gr n snk_id diff) rest (n+1)
            else ul_loop (new_arc gr n src_id (0.0 -. diff)) rest (n+1)
      in ul_loop ffgr ul 1

  in make_final_graph data

(** TODO:
    - Appliquer ff sur le graphe obtenu
    - Extraire et afficher les résultats
    cf. https://hackernoon.com/max-flow-algorithm-in-real-life-551ebd781b25
 *)

let share_from_file in_path out_path = 
  let gr = get_graph_from_file in_path in
  export out_path (gmap gr string_of_float)
