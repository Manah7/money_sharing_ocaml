open Gfile
open Tools
open Fordfulkerson
open Moneysharing

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) outfile(2) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)
  in

  let () = share_from_file infile outfile in


()
