open Csvparser
open Fordfulkerson
open Gfile
open Graph
open Printf
open Tools

let add_user line ul id = 
  try Scanf.sscanf line "p %s %f" (fun user amount -> (id, user, amount)::ul)
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
  | (_, _, a)::rest -> a +. (sum_amount rest)


(* Based on from_file() (gfile.ml) *)
let rec get_info_from_file path = 
  let infile = open_in path in

  (* Read all lines until end of file, altering graph and user list *)
  let rec loop graph ul =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      (* Calculing user id *)
      let user_id = (List.length ul) + 2 in

      (* We add user if there is one *)
      let ul2 =
        if line = "" then ul
        else match line.[0] with
          | 'p' -> add_user line ul user_id
          | _ -> read_comment ul line
      in

      (* We add user to graph *)
      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        else match line.[0] with
          | 'p' -> new_node graph user_id
          | _ -> read_comment graph line
      in

      loop graph2 ul2

    with End_of_file -> (graph, ul) (* Done *)
  in

  let data = loop empty_graph [] in


  (* Adding arcs depending on the total amount *)
  let make_final_graph data = match data with
    | (gr, ul) -> 
      (* Calculation of total amount *)
      let total = sum_amount ul in

      (* Calculation of user part *)
      let user_part = total /. (float_of_int (List.length ul)) in

      (* Printing some useful information *)
      Printf.printf "Total amount: %.2f\n" total;
      Printf.printf "Part of each user: %.2f\n" user_part;

      (* Adding infinte capacity arc to graph *)
      let fgr = complete_graph gr total in

      (* Add source and sink nodes *)
      let src_id = 0 in
      let snk_id = 1 in
      let ffgr = new_node (new_node fgr src_id) snk_id in

      (* Looping on user list to add outward arcs *)
      let rec ul_loop gr ul n = match ul with
        | [] -> gr
        | (_, name, a)::rest -> 
          let diff = a -. user_part in
          (* Printf.printf "[Debug] User: %s, amount: %.2f, diff: %.2f, n: %d\n" name a diff n; *)
          if diff > 0.0 then ul_loop (new_arc gr n snk_id diff) rest (n-1)
          else ul_loop (new_arc gr src_id n (0.0 -. diff)) rest (n-1)
      
      (* We add user_part in order to determine which arcs is useful *)
      in (ul_loop ffgr ul ((List.length ul)+1), ul)

  in make_final_graph data


let drop_threshold gr tshd = e_fold gr (fun tgr id1 id2 (x,y)-> if x = 0.0 || x > tshd then tgr else new_arc tgr id1 id2 (x,y)) (clone_nodes gr)

let rec get_name_opt ul id = match ul with
  | [] -> None
  | (id2, name, _)::rest -> if id = id2 then Some name else get_name_opt rest id

let get_name ul id = match get_name_opt ul id with
  | None -> ""
  | Some n -> n

let get_debts fgr ul name id = 
  let rec loop ul = 
  match ul with
    | [] -> ()
    | (id2, name, amn)::rest -> match find_arc fgr id2 id with
      | None -> loop rest 
      | Some (f, _) ->  Printf.printf "%s owes %.2f to %s\n" name f (get_name ul id); loop rest
  in loop ul

let rec print_ul ul = match ul with
  | [] -> Printf.printf "\n"
  | (id, name, _)::rest -> (* Printf.printf "%d: %s (recorded: %s)\n" id (get_name ul id) name; *) print_ul rest

let share_from_info info out_path = 
  (* Building graph from file and getting user_part as tshd *)
  match info with
  | (gr, ul) ->
    (* (re)calculing tshd *)
    let tshd = (sum_amount ul) /. (float_of_int (List.length ul)) in

    (* Applying ford fulkerson *)
    let src_id = 0 in
    let snk_id = 1 in
    let ffgr = ford_fulkerson_f gr src_id snk_id in

    (* Filtering useless arcs *)
    let final_gr = drop_threshold ffgr tshd in

    (* Exporting final graph in .svg *)
    export_ff_f out_path final_gr;

    (* Extracting data from graph *)
    print_ul ul;
    List.iter (fun (id, name, amn) -> 
      if id < 2 then ()
      else get_debts final_gr ul name id
    ) ul

let share_from_file in_path out_path =
  share_from_info (get_info_from_file in_path) out_path

let share_from_csv in_path out_path =
  share_from_info (get_info_from_csv in_path) out_path