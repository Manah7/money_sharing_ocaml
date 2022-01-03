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

let complete_graph gr = (* TODO *)
  let loop node = assert false in assert false


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

      (* If there is a new user, we increment the user count *)
      loop graph2 ul2

    with End_of_file -> (graph, ul) (* Done *)
  in


  let data = loop empty_graph user_list in

  (* Adding arcs depending on the total amount*)
  let make_final_graph data = match data with
    | (gr, ul) -> 
      let total = sum_amount ul in
      assert false

  in (* let ffgr = *) make_final_graph data

let share_from_file path = 
  let gr = get_graph_from_file path in
  assert false
