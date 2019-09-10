include "exemples/Caml Light/Rubik/cube4x4/divers/types.ml";;
include "exemples/Caml Light/Rubik/divers/divers.ml";;
include "exemples/Caml Light/Rubik/cube4x4/divers/section_marques.ml";;
include "exemples/Caml Light/Rubik/divers/couleurs.ml";;
include "exemples/Caml Light/Rubik/cube4x4/divers/graphisme.ml";;
include "exemples/Caml Light/Rubik/cube4x4/divers/boutons.ml";;


(* 'faces' renvoie une liste de couples : la première composante est un centre 'c' de minicube, la deuxième composante *)
(* est une liste des projections des contours des faces visibles du minicube centré en 'c' *)
let faces c =
	let face1 v c = map_vect (fun pt -> [|xx v pt; yy v pt|]) (face v c)
	in
		if est_coin c then
			c, map_vect (fun v -> face1 v c) [|[|c.(0) / 3; 0; 0|]; [|0; c.(1) / 3; 0|]; [|0; 0; c.(2) / 3|]|]
		else
			c, map_vect (fun v -> face1 v c) (vect_of_list (subtract [[|c.(0) / 3; 0; 0|]; [|0; c.(1) / 3; 0|]; [|0; 0; c.(2) / 3|]] [[|0; 0; 0;|]]))
;;

(* constitution d'une tableau de boutons de saisie de type 'btn' à partir des couples donnés pour chaque centre par la fonction 'faces' *)
(* chaque face visible de minicube devient un bouton de type 'btn' *)
(* aucune vérification n'est faite en cours de saisie ! *)
let btn_vect =
	let liste_indices = select (fun x -> x /|/ x <> 9) indices
	in
		let w = vect_of_list (map faces liste_indices)
		in
			map_vect
			(
				fun i ->
								{
									btn_centre = fst w.(i);
									btn_faces = snd w.(i);
									btn_couleurs = if est_coin (fst w.(i)) then
										[|"blanc"; "blanc"; "blanc"|]
									else if est_angle (fst w.(i)) then [|"blanc"; "blanc"|]
									else [|"blanc"|]
								}
			)
			(vect_of_list (liste (list_length liste_indices)))
;;

include "exemples/Caml Light/Rubik/divers/saisie.ml";;

boucle_saisie " 666x800";;


(*- création du mouvement à partir des informations collectées dans 'boucle_saisie' -*)

let mat indices_ref (x, c) =
	let mat1 x c =
		(* mouvement inverse du coin de couleurs c.(0) c.(1) c.(2) centré en x *)
		let v = [|[|x.(0) / 3; 0; 0|]; [|0; x.(1) / 3; 0|]; [|0; 0; x.(2) / 3|]|] in
			x, hd (select (fun m -> v.(0) /:/ m = face_de_nom c.(0) && v.(1) /:/ m = face_de_nom c.(1) && v.(2) /:/ m = face_de_nom c.(2)) groupe_du_cube)
	and mat2 x c =
		(* mouvement inverse de l'angle de couleurs c.(0) c.(1) centré en x *)
		let x1 = vect_of_list (subtract [[|x.(0) / 3; 0; 0|]; [|0; x.(1) / 3; 0|]; [|0; 0; x.(2) / 3|]] [[|0; 0; 0|]])
		and c1 = map_vect face_de_nom c
		in
			x, hd (select (fun m -> x1.(0) /:/ m = c1.(0) && x1.(1) /:/ m = c1.(1)) groupe_du_cube)
	and mat3 x c =
		(* mouvement inverse du milieu de couleur c.(0) centré en x *)
		(* inds est une référence aux indices des centres des minicubes *)
		let centre_orig c =
			let est_centre x = x /|/ x = 11 and encode x = [|x.(0) / 3; x.(1) / 3; x.(2) / 3|]
			in
				let a = hd (select (fun x -> est_centre x && encode x = face_de_nom c.(0)) !indices_ref) in
					indices_ref := subtract !indices_ref [a];
					a
		in
			let ct = centre_orig c in
				x, hd (select (fun m -> x /:/ m = ct) groupe_du_cube)
	in if x /|/ x = 27 then mat1 x c
		else if x /|/ x = 19 then mat2 x c
		else if x /|/ x = 11 then mat3 x c
		else x, identity 3
;;

let mouvement4x4 btn_vect =
	let mouvement4x4inverse btn_vect =
		let indices_ref = ref indices
		and aux = map_vect (fun x -> x.btn_centre, x.btn_couleurs) btn_vect
		in
			let l = ref [] in
				for i = 0 to vect_length aux - 1 do
					l := (mat indices_ref aux.(i)) :: !l
				done;
				map (fun x -> x, id) (list_of_vect (identity 3) @ list_of_vect (identity (- 3))) @ rev !l
	in
		inverse (mouvement4x4inverse btn_vect)
;;


(*

(* EXEMPLE 4x4 *)
(* aucune vérification n'a été faite en cours de saisie ! *)
let mouv4x4 = mouvement4x4 btn_vect;;
enregistrer mouv4x4 "mouv4x4-camllight";;
let mv1 = (lire_mouvement "mouv4x4-camllight" : (int vect * int vect vect) list);;

*)