(* on construit le toplevel caml_all au premier niveau du r�pertoire de WinCaml *)
(* Utiliser une distribution Caml Light compl�te *)
(* on suppose que MinGW est install� dans le r�pertoire C:\MinGW et MSYS dans le r�pertoire C:\msys *)
(* le script bash 'camlmktop' est sur le chemin 'PATH' d�fini par WinCaml *) 
sys__getenv "PATH";;
sys__system_command "c:/msys/1.0/bin/bash.exe -c \"PATH=/mingw/bin:/bin:$PATH;camlmktop -o caml_all.exe -custom int_misc.zo fnat.zo nat.zo big_int.zo arith_flags.zo ratio.zo num.zo arith_status.zo numprint.zo graphics.zo str.zo -lgraph -lnums -lstr -lkernel32 -lgdi32 -luser32\"";;
