include "exemples/Caml Light/Rubik/cube3x3/divers/mouvements.ml";;

(* morphisme 's: M -> S' et section 'l: S -> M' *)
(* construction d'une section 'l' de la suite exacte '0 -> K -> M -> S -> 0' *)
(* En Caml on représente la sujection 's' par 'sur', la section 'l' par 'sec' et 'gij' par 'gg i j' *)

let est_centre x = x /|/ x = 1;;
let est_angle x = x /|/ x = 2;;
let est_coin x = x /|/ x = 3;;


(* éléments g_{ij} alias gg i j de G servant à construire cette section *)
let gk i =
	let gka j = match vect j with
			| (0, 1, 1) (* kappa *) -> id
			| (1, 0, 1) -> rot [|0; 0; 1|] (* gamma *)
			| (0, - 1, 1) -> [|[|- 1; 0; 0|]; [|0; - 1; 0|]; [|0; 0; 1|]|] (* gamma^2 *)
			| (- 1, 0, 1) -> rot' [|0; 0; 1|] (* gamma^3 *)
			
			| (0, 1, - 1) -> rot [|1; 0; 0|]
			| (0, - 1, - 1) -> [|[|1; 0; 0|]; [|0; - 1; 0|]; [|0; 0; - 1|]|]
			| (- 1, 0, - 1) -> [|[|0; - 1; 0|]; [|- 1; 0; 0|]; [|0; 0; - 1|]|]
			| (1, 0, - 1) -> [|[|0; 1; 0|]; [|1; 0; 0|]; [|0; 0; - 1|]|]
			| (1, 1, 0) -> rot' [|0; 1; 0|]
			| (1, - 1, 0) -> [|[|0; 0; 1|]; [|0; - 1; 0|]; [|1; 0; 0|]|]
			| (- 1, - 1, 0) -> [|[|0; 0; - 1|]; [|0; - 1; 0|]; [|- 1; 0; 0|]|]
			| (- 1, 1, 0) -> rot [|0; 1; 0|]
			| _ -> failwith "gka"
	and gkc j = match vect j with
			| (1, 1, 1) (* kappa *) -> id
			| (1, - 1, 1) -> rot [|0; 0; 1|] (* gamma *)
			| (- 1, - 1, 1) -> [|[|- 1; 0; 0|]; [|0; - 1; 0|]; [|0; 0; 1|]|] (* gamma^2 *)
			| (- 1, 1, 1) -> rot' [|0; 0; 1|] (* gamma^3 *)
			
			| (- 1, - 1, - 1) -> [|[|0; 0; - 1|]; [|0; - 1; 0|]; [|- 1; 0; 0|]|] (* demi-tour / (1,0,-1) *)
			| (1, 1, - 1) -> rot [|1; 0; 0|]
			| (1, - 1, - 1) -> [|[|1; 0; 0|]; [|0; - 1; 0|]; [|0; 0; - 1|]|]
			| (- 1, 1, - 1) -> [|[|- 1; 0; 0|]; [|0; 1; 0|]; [|0; 0; - 1|]|]
			| _ -> failwith "gkc"
	in
		if est_angle i then gka i else if est_coin i then gkc i else id
;;

let gg i j = transpose (gk i) /./ gk j;;


(* stabilisateurs des angles et des coins *)
let st i =
	let sta i =
		let m = [|[|- 1; 0; 0|]; [|0; 0; 1|]; [|0; 1; 0|]|] (* h_kappa *)
		in
			transpose (gk i) /./ m /./ gk i
	and stc i =
		let m = [|[|0; 0; 1|]; [|1; 0; 0|]; [|0; 1; 0|]|] (* h_kappa *)
		in
			transpose (gk i) /./ m /./ gk i
	in
		if est_angle i then sta i else if est_coin i then stc i else failwith "st"
;;

include "exemples/Caml Light/Rubik/cube3x3/divers/section_commun.ml";;