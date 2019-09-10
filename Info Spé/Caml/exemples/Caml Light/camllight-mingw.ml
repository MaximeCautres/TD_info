(* On suppose MinGW installé dans C:\MinGW et msys dans C:\msys *)
(* Utiliser une distribution Caml Light complète *)
(* Le script bash 'camllight' est sur le chemin 'PATH' défini par WinCaml *)

let sys_command cmd = sys__system_command ("c:/msys/1.0/bin/bash -c " ^ cmd)
in
   sys_command "PATH=/c/MinGW/bin:/bin:$PATH;camllight"
;;
(* sélectionner et envoyer les 3 lignes suivantes... 
let f x = x*x;;
f 9;;
quit();;
*)