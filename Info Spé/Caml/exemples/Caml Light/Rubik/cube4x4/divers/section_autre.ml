include "exemples/Caml Light/Rubik/cube4x4/divers/mouvements.ml";;

let est_coin x = (x /|/ x) = 27;;
let est_angle x = (x /|/ x) = 19;;
let est_centre x = (x /|/ x) = 11;;


(* morphisme 's: M -> S' et section 'l: S -> M' *)
(* construction d'une section 'l' de la suite exacte '0 -> K -> M -> S -> 0' *)
(* En Caml on représente la sujection 's' par 'sur', la section 'l' par 'sec' et 'gij' par 'gg i j' *)

(* éléments g_{ij} alias gg i j de G servant à construire cette section *)
let gg i j =
	if i = j then id
	else if est_coin i && est_coin j then
		let (a, b, c) = (i.(0) * j.(0) / 9, i.(1) * j.(1) / 9, i.(2) * j.(2) / 9)
		and m = make_matrix 3 3 0 in
			m.(0).(0) <- a;
			if a * b * c = 1 then (
					m.(1).(1) <- b;
					m.(2).(2) <- c
				)
			else (
					m.(2).(1) <- i.(2) * j.(1) / 9;
					m.(1).(2) <- i.(1) * j.(2) / 9
				);
			m
	else if est_angle i && est_angle j || est_centre i && est_centre j then
		hd (select (fun g -> i /:/ g = j) groupe_du_cube)
	else id
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
	let (a, b, c) = vect i in
		let m = [|[|0; a * b / 9; 0|]; [|0; 0; b * c / 9|]; [|c * a / 9; 0; 0|]|] in
			if a * b * c > 0 (* cohérence des sens de rotation *)
			then transpose m
			else m
;;
