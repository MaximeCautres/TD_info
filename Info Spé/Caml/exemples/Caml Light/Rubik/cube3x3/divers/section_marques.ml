include "exemples/Caml Light/Rubik/cube3x3/divers/mouvements.ml";;

(* morphisme 's: M -> S' et section 'l: S -> M' *)
(* construction d'une section 'l' de la suite exacte '0 -> K -> M -> S -> 0' *)
(* En Caml on représente la sujection 's' par 'sur', la section 'l' par 'sec' et 'gij' par 'gg i j' *)

let est_centre x = x /|/ x = 1;;
let est_angle x = x /|/ x = 2;;
let est_coin x = x /|/ x = 3;;

let marque x =
	if est_coin x then [|0; 0; x.(2)|]
	else if est_angle x then let a, b, c = x.(0), x.(1), x.(2)
		in match a, b, c with
				| 0, _, _ -> [|0; b; 0|]
				| _, 0, _ -> [|0; 0; c|]
				| _, _, 0 -> [|a; 0; 0|]
				| _ -> [|0; 0; 0|]
	else [|0; 0; 0|]
;;


(* éléments g_{ij} alias gg i j de G servant à construire cette section *)
let gg i j =
	let critere i j g = i /:/ g = j && marque i /:/ g = marque j
	in
		hd (select (critere i j) groupe_du_cube)
		(* cette liste devrait toujours contenir exactement un élément *)
;;


(* stabilisateurs des angles et des coins *)
let st i =
	let stc i =
		let x = [|1; 1; 1|]
		and m = [|[|0; 0; 1|]; [|1; 0; 0|]; [|0; 1; 0|]|]
		in
			gg i x /./ m /./ gg x i
	and sta i = let x = [|1; 0; 1|]
		and m = [|[|0; 0; 1|]; [|0; - 1; 0|]; [|1; 0; 0|]|]
		in
			gg i x /./ m /./ gg x i
	in
		if est_angle i then sta i else if est_coin i then stc i else failwith "st"
;;

include "exemples/Caml Light/Rubik/cube3x3/divers/section_commun.ml";; 