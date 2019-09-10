(* Linux, Mac OS ou Windows avec Cygwin *)
(* Utiliser une distribution Caml Light complète *)

cd "exemples/Caml Light/inria/calculette/calc";;
sys__system_command "./build-calculette";;
sys__system_command "./cleanup";;
load_object "libcalculette.zo";;

(* "Envoyer" pour effectuer les opérations arithmétiques suivantes *)
(* "Interrompre" pour arrêter la calculette *)
(7 + 5) * - 8 / 4
(124 / 2) / 2