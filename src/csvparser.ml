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

(* (id * name * amount) list -> name -> id option *)
let rec get_id_from_name ul name = match ul with
  | [] -> None
  | (id, n, _)::rest -> if n = name then Some id else get_id_from_name rest name

(* Return a new list with updated amount for one given id *)
let rec add_amount_to_id ul id a = 
  List.map (fun (id2, name, amount) -> 
      if id = id2 then (id2, name, (amount +. a)) else (id2, name, amount)) ul

(* Return the number of part in a line *)
let rec get_total_part = function
  | [] -> 0
  | s::r -> if s = "" then get_total_part r else (int_of_string s)+(get_total_part r)




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
          | name::rest -> (id, name, 0.0)::(extract_name (id+1) rest)
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
              | None -> Printf.printf "Unknow payer in list (%s), skipping..." payer; csv_error "TODO"
              | Some id -> id) (float_of_string s_amount) in

          (* Rest of the line analysis - Removing amount to other participants *)
          let nb_part = get_total_part rest in
          let part = (float_of_string s_amount) /. (float_of_int nb_part) in

          let rec loop_participants ul3 coef_line id = match coef_line with
            | [] -> ul3
            | ""::r -> loop_participants ul3 r (id+1)
            (* Si la case coef. n'est pas vide, on enlève part*coef. à l'user *)
            | c::r -> loop_participants (
                add_amount_to_id ul3 id (0.0-.((float_of_int (int_of_string c))*.part))
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

  (empty_graph, ul_with_amount)

  (* TODO : parse ul and add mssing arcs *)

(**********************************************)


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
