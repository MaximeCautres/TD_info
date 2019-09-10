(* construit le toplevel caml_all au premier niveau du répertoire de WinCaml *)
(* Utiliser une distribution Caml Light complète *)
(* le script bash 'camlmktop' est sur le chemin 'PATH' défini par WinCaml *)
sys__system_command "camlmktop -o caml_all.exe -custom int_misc.zo fnat.zo nat.zo big_int.zo arith_flags.zo ratio.zo num.zo arith_status.zo numprint.zo unix.zo graphics.zo str.zo -lunix -lgraph -lnums -lstr -lkernel32 -lgdi32 -luser32";;
