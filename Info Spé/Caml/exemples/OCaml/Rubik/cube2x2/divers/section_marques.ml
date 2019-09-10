#use "exemples/OCaml/Rubik/cube2x2/divers/mouvements.ml";;

let marque x = [|0; 0; x.(2)|];;


(* morphisme 's: M -> S' et section 'l: S -> M' *)
(* construction d'une section 'l' de la suite exacte '0 -> K -> M -> S -> 0' *)
(* En Caml on repr�sente la sujection 's' par 'sur', la section 'l' par 'sec' et 'gij' par 'gg i j' *)

(* �l�ments g_{ij} alias gg i j de G servant � construire cette section *)
let gg i j =
	try
		let critere i j g = i /:/ g = j && marque i /:/ g = marque j
		in
			hd (select (critere i j) groupe_du_cube)
	with Failure "hd" -> id
					(* cette liste devrait toujours contenir exactement un �l�ment *)
;;

(* stabilisateur du coin i *)
let st i =
	let k = [|1; 1; 1|] and m = [|[|0; 0; 1|]; [|1; 0; 0|]; [|0; 1; 0|]|] (* h_kappa pour C *)
	in
		gg i k /./ m /./ gg k i
;;

#use "exemples/OCaml/Rubik/cube2x2/divers/section_commun.ml";;