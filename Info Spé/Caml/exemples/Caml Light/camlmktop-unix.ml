(* construit le toplevel caml_all au premier niveau du répertoire MacCaml *)
sys__system_command "camlmktop -o caml_all -custom int_misc.zo fnat.zo nat.zo big_int.zo arith_flags.zo ratio.zo num.zo arith_status.zo numprint.zo unix.zo graphics.zo str.zo -ccopt -L/opt/X11/lib -lX11 -lpthread -lunix -lgraph -lnums -lstr";;
