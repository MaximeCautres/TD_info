include "exemples/Caml Light/Rubik/cube3x3/divers/types.ml";;
include "exemples/Caml Light/Rubik/divers/divers.ml";;
include "exemples/Caml Light/Rubik/cube3x3/divers/section.ml";;
include "exemples/Caml Light/Rubik/divers/couleurs.ml";;
include "exemples/Caml Light/Rubik/cube3x3/divers/graphisme.ml";;
include "exemples/Caml Light/Rubik/cube3x3/divers/boutons.ml";;

(*- boutons de saisie d'une configuration du cube 3x3 � partir des couleurs des faces visibles des minicubes -*)

(* 'faces' renvoie une liste de couples : la premi�re composante est un indice 'c', la deuxi�me composante *)
(* est une liste des projections des contours des faces visibles du minicube centr� en 'c' *)
let faces c =
	let face1 v c = map_vect (fun pt -> [|xx v pt; yy v pt|]) (face v c)
	in
		if est_coin c then
			c, map_vect (fun v -> face1 v c) [|[|c.(0); 0; 0|]; [|0; c.(1); 0|]; [|0; 0; c.(2)|]|]
		else
			c, map_vect (fun v -> face1 v c) (vect_of_list (subtract [[|c.(0); 0; 0|]; [|0; c.(1); 0|]; [|0; 0; c.(2)|]] [[|0; 0; 0|]]))
;;

(* constitution d'une tableau de boutons de type 'btn' � partir des couples donn�s pour chaque indice par la fonction 'faces' *)
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
								btn_couleurs = if est_coin (fst w.(i)) then
									[|"blanc"; "blanc"; "blanc"|]
								else if est_angle (fst w.(i)) then [|"blanc"; "blanc"|]
								else [|"blanc"|]
							}
		)
		(vect_of_list (liste (list_length indices)))
;;

include "exemples/Caml Light/Rubik/divers/saisie.ml";;


boucle_saisie " 612x612";;


(*- cr�ation du mouvement � partir des informations collect�es dans 'boucle_saisie' -*)

(* mouvement inverse du minicube de couleurs parmi c.(0) c.(1) c.(2) centr� en x *)
let mat (x, c) =
	let v = subtract [[|x.(0); 0; 0|]; [|0; x.(1); 0|]; [|0; 0; x.(2)|]] [[|0; 0; 0|]]
	and cl = map face_de_nom (list_of_vect c)
	in
		hd (select (fun m -> it_list (prefix &&) true (map2 (fun x y -> x /:/ m = y) v cl)) groupe_du_cube)
;;

let mouvement3x3 btn_vect =
	let mouvement3x3inverse btn_vect =
		let aux = map_vect (fun x -> x.btn_centre, x.btn_couleurs) btn_vect
		in
			let l = ref [] in
				for i = 0 to vect_length aux - 1 do
					l := (fst aux.(i), mat aux.(i)) :: !l
				done;
				rev !l
	in
		inverse (mouvement3x3inverse btn_vect)
;;

(* adapter... en sorte que la r�solution compl�te s'applique *)
(* aussi dans le cas o� la permutation des coins est impaire *)
let adapter mv1 =
	let l = select (fun x -> fst x /|/ fst x = 3) mv1
	in
		if sign (map fst l) (sur mv1) = 1 then mv1
		else map (fun (x, m) -> if x <> [|0; 0; 1|] then (x, m) else (x, rot [|0; 0; 1|])) mv1
;;

(*

(* EXEMPLE 3x3 *)
(* aucune v�rification n'a �t� faite en cours de saisie ! *)
try
  let mv1 = adapter (mouvement3x3 btn_vect)
  in
    enregistrer mv1 "mouv3x3-camllight"
with Failure "hd" -> ()
;;
let mv1 = (lire_mouvement "mouv3x3-camllight" : (int vect * int vect vect) list);;

*)