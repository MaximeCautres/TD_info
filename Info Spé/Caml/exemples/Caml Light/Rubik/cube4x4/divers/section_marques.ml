include "exemples/Caml Light/Rubik/cube4x4/divers/mouvements.ml";;

let est_coin x = (x /|/ x) = 27;;
let est_angle x = (x /|/ x) = 19;;
let est_centre x = (x /|/ x) = 11;;

let marque x =
	if est_coin x then [|0; 0; x.(2)|]
	else [|0; 0; 0|]
;;


(* morphisme 's: M -> S' et section 'l: S -> M' *)
(* construction d'une section 'l' de la suite exacte '0 -> K -> M -> S -> 0' *)
(* En Caml on représente la sujection 's' par 'sur', la section 'l' par 'sec' et 'gij' par 'gg i j' *)

(* éléments g_{ij} alias gg i j de G servant à construire cette section *)
let gg i j =
	let critere i j g = if est_coin i && est_coin j then i /:/ g = j && marque i /:/ g = marque j else i /:/ g = j
	in
		hd (select (critere i j) groupe_du_cube)
		(* cette liste devrait toujours contenir exactement un élément *)
;;


(* décomposition 'm = ker m /*/ sec (sur m)' d'un mouvement 'm' *)
(* avec 'ker m' élément du noyau de 'sur' *)
(* 'p' pour 'permutation': 'p = sur m' est la permutation 'p' des indices associée au mouvement 'm' *)
let sec p = mv1_of_fun (fun i -> gg i (p i));;
let sur m = fun i -> i /:/ fun_of_mv1 m i;;
let ker m = m /*/ inverse (sec (sur m));;


(* stabilisateur du coin i *)
(* les stabilisateurs des angles et des centres sont triviaux *)
let st i =
	let m = [|[|0; 0; 1|]; [|1; 0; 0|]; [|0; 1; 0|]|]
	and j = [|3; 3; 3|]
	in
		if est_coin i then
			gg i j /./ m /./ gg j i
		else failwith "st"
;;
