open Graph
open Printf

let make_default_csv path =
  let ff = open_out path in
  fprintf ff "Who paid?, How much?, user1, user2" ;
  fprintf ff "\n" ;
  close_out ff

let make_example_csv =
  let ff = open_out "example.csv" in
  fprintf ff "Who paid?, How much?, Alice, Bob, Carol, Dan" ;
  fprintf ff "\n" ;
  fprintf ff "Alice, 50, 1, 1, , 1" ;
  fprintf ff "\n" ;
  fprintf ff "Bob, 20, , 1, 1, 1" ;
  fprintf ff "\n" ;
  close_out ff

let csv_error str = 
  let stack = Printexc.get_backtrace () in
  Printf.eprintf "\nCSV error: %s\n %s\n" str stack;
  failwith "Exiting"

(* Add every possible outgoing arc from one given node with label lbl *)
let add_all_arcs_from_node gr id lbl = 
  n_fold gr (fun tgr id2 -> if id != id2 then new_arc tgr id id2 lbl else tgr) gr

(* Make a given graph complete by adding all possible outgoing arcs for every node *)
let complete_graph gr lbl = 
  n_fold gr (fun tgr id -> add_all_arcs_from_node tgr id lbl) gr

(* (id * name * amount) list -> name -> id option *)
let rec get_id_from_name ul name = match ul with
  | [] -> None
  | (id, n, _)::rest -> Printf.printf "[debug] Searching %s: Found %s (id: %d) \n" name n id;
    if n = name then Some id else get_id_from_name rest name

(* Return a new list with updated amount for one given id *)
let rec add_amount_to_id ul id a = 
  List.map (fun (id2, name, amount) -> 
      if id = id2 then (id2, name, (amount +. a)) else (id2, name, amount)) ul

(* Return the number of part in a line *)
let rec get_total_part = function
  | [] -> 0
  | s::r -> Printf.printf "[debug] Calculing total part, found: %s \n" (String.trim s);
    if s = " " || s = "" then get_total_part r else (int_of_string (String.trim s))+(get_total_part r)

let rec create_graph_from_ul = function
  | [] -> empty_graph
  | (id, name, _)::rest -> Printf.printf "[debug] Created a node for %s (id: %d)\n" name id;
    new_node (create_graph_from_ul rest) id

(* Print a given user list - useful for debug *)
let rec print_ul = function
  | [] -> Printf.printf "[info] Done.\n"
  | (id, name, a)::rest -> Printf.printf "[info] Name: %s, credit: %.2f\n" name a; print_ul rest

let rec get_info_from_csv path = 
  let infile = open_in path in

  (* Creating user list *)
  let ul = 
    try
      let line = String.trim (input_line infile) in

      (* We parse the first line to extract users (and we create ids) *)
      let line_split = String.split_on_char ',' line in
      match line_split with
      (* Skipping first two columns *)
      | whopaid::howmuch::name_list -> 
        let rec extract_name id = function
          | [] -> []
          | name::rest -> (id, (String.trim name), 0.0)::(extract_name (id+1) rest)
        in
        extract_name 2 name_list
      | _ -> csv_error "Invalid CSV file (first line parsing)."

    with End_of_file -> csv_error "Empty file."
  in

  (* Read all lines until end of file, altering  user list *)
  let rec loop ul =
    try
      let line = String.trim (input_line infile) in
      let line_split = String.split_on_char ',' line in

      let rec loop_line ul = function
        (* Getting payer and amount *)
        | payer::s_amount::rest -> 

          (* First two rows analysis - Adding amount to payer *)
          let ul2 = add_amount_to_id ul (match get_id_from_name ul payer with
              | None -> Printf.printf "Unknow payer in list (%s for %s), skipping..." payer s_amount; csv_error "TODO"
              | Some id -> id) (float_of_string s_amount) in

          (* Rest of the line analysis - Removing amount to other participants *)
          let nb_part = get_total_part rest in
          let part = (float_of_string s_amount) /. (float_of_int nb_part) in

          Printf.printf "[debug] Get total amount: %s \n" s_amount;
          Printf.printf "[debug] Calculed user part: %.2f \n" part;

          let rec loop_participants ul3 coef_line id = match coef_line with
            | [] -> ul3
            (* Si la case coef. n'est pas vide, on enlève part*coef. à l'user *)
            | c::r -> if c = "" || c = " " then loop_participants ul3 r (id+1) else
              loop_participants (
                add_amount_to_id ul3 id (0.0-.((float_of_int (int_of_string (String.trim c)))*.part))
              ) r (id+1)
          in

          loop_participants ul2 rest 2 (* Parsing coef. *)

        | _ -> csv_error "File badly formatted."
      in

      let ul4 = loop_line ul line_split in

      loop ul4 (* Going next line *)

    with End_of_file -> ul (* Parsing done. *)
  in

  let ul_with_amount = loop ul in

  print_ul ul_with_amount;

  (* Creating a graph, adding a node per user and complete the graph *)
  let graph = complete_graph (create_graph_from_ul ul_with_amount) Float.infinity in

  (* Add source and sink nodes *)
  let src_id = 0 in
  let snk_id = 1 in
  let fgr = new_node (new_node graph src_id) snk_id in

  (* Looping on user list to add outward arcs *)
  let rec ul_loop gr ul n = match ul with
    | [] -> gr
    | (_, name, a)::rest -> 
      if a > 0.0 then ul_loop (new_arc gr n snk_id a) rest (n+1)
      else ul_loop (new_arc gr src_id n (0.0 -. a)) rest (n+1)
  in 
  
  (ul_loop fgr ul_with_amount 2, ul_with_amount)


(**
   [Not intended for production purposes]
   Create an exmple CSV, run using:
    $ ocamlbuild csvparser.native; ./csvparser.native 
   Open with:
    $ libreoffice example.csv
 **)
let () =
  Printf.printf "Creating CSV example...";
  make_example_csv;
  Printf.printf "done.\n"
