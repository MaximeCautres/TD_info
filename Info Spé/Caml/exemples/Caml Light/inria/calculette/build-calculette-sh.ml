(* Linux, Mac OS ou Windows avec Cygwin *)
(* Utiliser une distribution Caml Light compl�te *)

cd "exemples/Caml Light/inria/calculette/calc";;
sys__system_command "./build-calculette";;
sys__system_command "./cleanup";;
load_object "libcalculette.zo";;

(* "Envoyer" pour effectuer les op�rations arithm�tiques suivantes *)
(* "Interrompre" pour arr�ter la calculette *)
(7 + 5) * - 8 / 4
(124 / 2) / 2