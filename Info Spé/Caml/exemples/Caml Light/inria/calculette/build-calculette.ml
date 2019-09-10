let (camllex, camlyacc, camllibr) =
  let run cmd f = sys__system_command (cmd ^ " " ^ f)
  in
    (run "camllex", run "camlyacc", run "camllibr")
;;

cd "exemples/Caml Light/inria/calculette/calc";;

camllex "lexer.mll";;
camlyacc "parser.mly";;
compile "parser.mli";;
compile "lexer.ml";;
compile "parser.ml";;
compile "calculette.ml";;
camllibr "-o libcalculette.zo lexer.zo parser.zo calculette.zo";;

load_object "libcalculette.zo";;

(* "Envoyer" pour effectuer les opérations arithmétiques suivantes *)
(* "Interrompre" pour arrêter la calculette *)
(7 + 5) * - 8 / 4
(124 / 2) / 2