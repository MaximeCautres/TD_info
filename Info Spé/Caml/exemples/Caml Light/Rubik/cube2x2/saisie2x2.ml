include "exemples/Caml Light/Rubik/cube2x2/divers/types.ml";;
include "exemples/Caml Light/Rubik/divers/divers.ml";;
include "exemples/Caml Light/Rubik/cube2x2/divers/section_autre.ml";;
include "exemples/Caml Light/Rubik/divers/couleurs.ml";;
include "exemples/Caml Light/Rubik/cube2x2/divers/graphisme.ml";;
include "exemples/Caml Light/Rubik/cube2x2/divers/boutons.ml";;

(*- boutons de saisie d'une configuration du cube 2x2 � partir des couleurs des faces visibles des minicubes -*)

(* 'faces' renvoie une liste de couples : la premi�re composante est un centre 'c', la deuxi�me composante *)
(* est une liste des projections des contours des faces visibles du minicube centr� en 'c' *)
let faces c =
	let face1 v c = map_vect (fun pt -> [|xx v pt; yy v pt|]) (face v c)
	in
		c, map_vect (fun v -> face1 v c) [|[|c.(0); 0; 0|]; [|0; c.(1); 0|]; [|0; 0; c.(2)|]|]
;;

(* constitution d'une tableau de boutons de type 'btn' � partir des couples donn�s pour chaque centre par la fonction 'faces' *)
(* chaque face visible de minicube devient un bouton de type 'btn' *)
(* aucune v�rification n'est faite en cours de saisie ! *)
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


(*- cr�ation du mouvement � partir des informations collect�es dans 'boucle_saisie' -*)

(* mouvement inverse du minicube de couleurs c.(0) c.(1) c.(2) centr� en x *)
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
(* aucune v�rification n'a �t� faite en cours de saisie ! *)
let mv1 = mouvement2x2 btn_vect;;
enregistrer mv1 "mouv2x2-camllight";;
enregistrer mouv4x4 "mouv4x4-camllight";;
let mv1 = (lire_mouvement "mouv2x2-camllight" : (int vect * int vect vect) list);;

*)