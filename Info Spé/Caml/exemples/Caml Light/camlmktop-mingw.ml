(* on construit le toplevel caml_all au premier niveau du répertoire de WinCaml *)
(* Utiliser une distribution Caml Light complète *)
(* on suppose que MinGW est installé dans le répertoire C:\MinGW et MSYS dans le répertoire C:\msys *)
(* le script bash 'camlmktop' est sur le chemin 'PATH' défini par WinCaml *) 
sys__getenv "PATH";;
sys__system_command "c:/msys/1.0/bin/bash.exe -c \"PATH=/mingw/bin:/bin:$PATH;camlmktop -o caml_all.exe -custom int_misc.zo fnat.zo nat.zo big_int.zo arith_flags.zo ratio.zo num.zo arith_status.zo numprint.zo graphics.zo str.zo -lgraph -lnums -lstr -lkernel32 -lgdi32 -luser32\"";;
