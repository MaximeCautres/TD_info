(* On suppose MinGW install� dans C:\MinGW et msys dans C:\msys *)
(* Utiliser une distribution Caml Light compl�te *)
(* Le script bash 'camllight' est sur le chemin 'PATH' d�fini par WinCaml *)

let sys_command cmd = sys__system_command ("c:/msys/1.0/bin/bash -c " ^ cmd)
in
   sys_command "PATH=/c/MinGW/bin:/bin:$PATH;camllight"
;;
(* s�lectionner et envoyer les 3 lignes suivantes... 
let f x = x*x;;
f 9;;
quit();;
*)