(* Test *)

open Abbrevs

let trim_newlines str =
  let rec aux filtered k =
    if k = S.length str then filtered
    else
      let a = str.[k] in
      match a with
      | '\n' -> aux (filtered ^ "@") (k+1)
      | _    -> aux (filtered ^ (S.of_char a)) (k+1)
  in
  aux "" 0

let analyze_commands ?(web = false) cmds =
  if web then
    L.iter (L.rev (Parse.p_cmds cmds))
           ~f:(fun cmd ->
             Unification.fresh_idx := 0;
             F.printf "#@";
             Eval.process_command cmd;
             F.print_flush()
           )
  else (L.iter (Parse.p_cmds cmds) ~f:(fun cmd -> Eval.process_command cmd; F.print_flush()); exit 0)

let rec analyze () =
  let input = Stdlib.read_line () in
  (try
     analyze_commands ~web:true input;
     F.printf "\n";
     F.print_flush();
   with
   | _ -> F.printf "&\\textsf{Syntax error}@@@\n"; F.print_flush()
  (*err -> F.printf "%s\n" (trim_newlines (Printexc.to_string err)); F.print_flush()*)
  );
  analyze()

let test =
  if Array.length Sys.argv = 1 then analyze ()
  else if Array.length Sys.argv = 2 then analyze_commands (Analyze.input_file Sys.argv.(1))
  else output_string stderr (F.sprintf "usage: %s <scheme file>\n" Sys.argv.(0))
