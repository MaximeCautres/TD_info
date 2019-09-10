let coeff = ref 1;;

include "exemples/Caml Light/Rubik/divers/graphisme.ml";;

(* 'dessine_cube context mv1' dessine le cube dans l'état 'mv1' *)
let dessine_cube context mv1 =
	let p = context.matrice and m = mv1 in
	(*faces frontales*)
		do_vect (fun x -> draw_face (proj_face p m x)) id;
		(*faces opposées*)
		do_vect (fun x -> draw_face (proj_face p m x)) idm;
;;
