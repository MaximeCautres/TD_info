#use "exemples/OCaml/Rubik/cube2x2/divers/mouvements.ml";;


(* morphisme 's: M -> S' et section 'l: S -> M' *)
(* construction d'une section 'l' de la suite exacte '0 -> K -> M -> S -> 0' *)
(* En Caml on représente la sujection 's' par 'sur', la section 'l' par 'sec' et 'gij' par 'gg i j' *)

(* éléments g_{ij} alias gg i j de G servant à construire cette section *)

let gg i j =
	let (a, b, c) = (i.(0) * j.(0), i.(1) * j.(1), i.(2) * j.(2))
	and m = make_matrix 3 3 0 in
		m.(0).(0) <- a;
		if a * b * c = 1 then (
				m.(1).(1) <- b;
				m.(2).(2) <- c
			)
		else (
				m.(2).(1) <- i.(2) * j.(1);
				m.(1).(2) <- i.(1) * j.(2)
			);
		m
;;

(* stabilisateur du coin i *)
let st i =
	let (a, b, c) = vect i in
		let m = [|[|0; a * b; 0|]; [|0; 0; b * c|]; [|c * a; 0; 0|]|] in
			if a * b * c > 0 (* cohérence des sens de rotation *)
			then transpose m
			else m
;;

#use "exemples/OCaml/Rubik/cube2x2/divers/section_commun.ml";;