(* On suppose MinGW installé dans C:\MinGW et msys dans C:\msys *)
(* Utiliser une distribution Caml Light complète *)

let sys_command cmd = sys__system_command ("c:/msys/1.0/bin/bash -c " ^ cmd)
in
   cd "exemples/Caml Light/inria/calculette/calc";
   sys_command "PATH=.:/c/MinGW/bin:/bin:$PATH;build-calculette;cleanup"
;;
load_object "libcalculette.zo";;

(* "Envoyer" pour effectuer les opérations arithmétiques suivantes *)
(* "Interrompre" pour arrêter la calculette *)
(7 + 5) * - 8 / 4
(124 / 2) / 2