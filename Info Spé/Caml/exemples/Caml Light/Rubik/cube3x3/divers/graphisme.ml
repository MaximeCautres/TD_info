let coeff = ref 2;;

include "exemples/Caml Light/Rubik/divers/graphisme.ml";;

(* 'draw_face1' ajoute à 'draw_face' le tracé de segments permettant de suivre *)
(* la rotation des centres *)
let draw_face1 p m x =
	draw_face (proj_face p m x);
	let y = x /:/ p in
		let v = map_vect (fun pt -> (xx y pt, yy y pt)) (map_vect (fun t -> t /:/ p) (face x x))
		in
			let ((a1, b1), (a2, b2), (a3, b3), (a4, b4)) = (v.(0), v.(1), v.(2), v.(3))
			in
				let (c1, c2) = ((a1 + a3) / 2, (b1 + b3) / 2)
				in
					graphics__moveto c1 c2;
					graphics__set_color (graphics__rgb 120 120 120);
					graphics__lineto a1 b1;
					graphics__set_color graphics__black;
					graphics__moveto c1 c2;
					let mat = assoc x m in
						if mat = id then graphics__lineto a1 b1
						else if mat = rot x then graphics__lineto a2 b2
						else if mat = rot x /./ rot x then graphics__lineto a3 b3
						else graphics__lineto a4 b4


;;

(* 'dessine_cube context mv1' dessine le cube dans l'état 'mv1' *)
let dessine_cube context mv1 =
	let p = context.matrice and m = mv1 in
	(*faces frontales*)
		do_vect (fun x -> draw_face1 p m x) id;
		(*faces opposées*)
		do_vect (fun x -> draw_face1 p m x) idm;
;;

