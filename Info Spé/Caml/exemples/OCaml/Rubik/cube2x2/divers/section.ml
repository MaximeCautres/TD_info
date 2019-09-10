#use "exemples/OCaml/Rubik/cube2x2/divers/mouvements.ml";;

(* morphisme 's: M -> S' et section 'l: S -> M' *)
(* construction d'une section 'l' de la suite exacte '0 -> K -> M -> S -> 0' *)
(* En Caml on représente la sujection 's' par 'sur', la section 'l' par 'sec' et 'gij' par 'gg i j' *)

(* éléments g_{ij} alias gg i j de G servant à construire cette section *)
let gk i =
	match vect i with
		| (1, 1, 1) (* kappa *) -> id
		| (1, - 1, 1) -> rot [|0; 0; 1|] (* gamma *)
		| (- 1, - 1, 1) -> [|[|- 1; 0; 0|]; [|0; - 1; 0|]; [|0; 0; 1|]|] (* gamma^2 *)
		| (- 1, 1, 1) -> rot' [|0; 0; 1|] (* gamma^3 *)
		
		| (- 1, - 1, - 1) -> [|[|0; 0; - 1|]; [|0; - 1; 0|]; [|- 1; 0; 0|]|] (* demi-tour / (1,0,-1) *)
		| (1, 1, - 1) -> rot [|1; 0; 0|]
		| (1, - 1, - 1) -> [|[|1; 0; 0|]; [|0; - 1; 0|]; [|0; 0; - 1|]|]
		| (- 1, 1, - 1) -> [|[|- 1; 0; 0|]; [|0; 1; 0|]; [|0; 0; - 1|]|]
		| _ -> failwith "gk"
;;

let gg i j = transpose (gk i) /./ gk j;;

(* stabilisateur du coin i *)
let st i =
	let m = [|[|0; 0; 1|]; [|1; 0; 0|]; [|0; 1; 0|]|] (* h_kappa *)
	in
		transpose (gk i) /./ m /./ gk i
;;

#use "exemples/OCaml/Rubik/cube2x2/divers/section_commun.ml";;