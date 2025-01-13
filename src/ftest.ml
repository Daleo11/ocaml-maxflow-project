open Gfile
open Algo
open Tools
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph in


  (*
  let gr_sortie=floyd (gmap graph int_of_string) _source _sink in
  let () = write_file outfile (gmap gr_sortie string_of_int) in
  () ;;
   *)
Printf.printf("avant l'erreur \n");
let visited=bfs (gmap graph int_of_string) 0 in
Printf.printf("je suis ici");
let rec aff_visited v=
match v with
|(n,_)::rest->Printf.printf "%d " n ; aff_visited rest
|[]->Printf.printf " "
in
aff_visited visited;
()
;;
