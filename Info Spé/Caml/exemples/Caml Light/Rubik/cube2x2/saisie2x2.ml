include "exemples/Caml Light/Rubik/cube2x2/divers/types.ml";;
include "exemples/Caml Light/Rubik/divers/divers.ml";;
include "exemples/Caml Light/Rubik/cube2x2/divers/section_autre.ml";;
include "exemples/Caml Light/Rubik/divers/couleurs.ml";;
include "exemples/Caml Light/Rubik/cube2x2/divers/graphisme.ml";;
include "exemples/Caml Light/Rubik/cube2x2/divers/boutons.ml";;

(*- boutons de saisie d'une configuration du cube 2x2 à partir des couleurs des faces visibles des minicubes -*)

(* 'faces' renvoie une liste de couples : la première composante est un centre 'c', la deuxième composante *)
(* est une liste des projections des contours des faces visibles du minicube centré en 'c' *)
let faces c =
	let face1 v c = map_vect (fun pt -> [|xx v pt; yy v pt|]) (face v c)
	in
		c, map_vect (fun v -> face1 v c) [|[|c.(0); 0; 0|]; [|0; c.(1); 0|]; [|0; 0; c.(2)|]|]
;;

(* constitution d'une tableau de boutons de type 'btn' à partir des couples donnés pour chaque centre par la fonction 'faces' *)
(* chaque face visible de minicube devient un bouton de type 'btn' *)
(* aucune vérification n'est faite en cours de saisie ! *)
let btn_vect =
	let w = vect_of_list (map faces indices)
	in
		map_vect
		(
			fun i ->
							{
								btn_centre = fst w.(i);
								btn_faces = snd w.(i);
								btn_couleurs = [|"blanc"; "blanc"; "blanc"|]
							}
		)
		(vect_of_list (liste (list_length indices)))
;;

include "exemples/Caml Light/Rubik/divers/saisie.ml";;


boucle_saisie " 612x612";;


(*- création du mouvement à partir des informations collectées dans 'boucle_saisie' -*)

(* mouvement inverse du minicube de couleurs c.(0) c.(1) c.(2) centré en x *)
let mat (x, c) =
	let v = subtract [[|x.(0); 0; 0|]; [|0; x.(1); 0|]; [|0; 0; x.(2)|]] [[|0; 0; 0|]]
	and cl = map face_de_nom (list_of_vect c)
	in
		hd (select (fun m -> it_list (prefix &&) true (map2 (fun x y -> x /:/ m = y) v cl)) groupe_du_cube)
;;

let mouvement2x2 btn_vect =
	let mouvement2x2inverse btn_vect =
		let aux = map_vect (fun x -> x.btn_centre, x.btn_couleurs) btn_vect
		in
			let l = ref [] in
				for i = 0 to vect_length aux - 1 do
					l := (fst aux.(i), mat aux.(i)) :: !l
				done;
				rev !l
	in
		inverse (mouvement2x2inverse btn_vect)
;;

(*

(* EXEMPLE 2x2 *)
(* aucune vérification n'a été faite en cours de saisie ! *)
let mv1 = mouvement2x2 btn_vect;;
enregistrer mv1 "mouv2x2-camllight";;
enregistrer mouv4x4 "mouv4x4-camllight";;
let mv1 = (lire_mouvement "mouv2x2-camllight" : (int vect * int vect vect) list);;

*)