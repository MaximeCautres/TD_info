#use "exemples/OCaml/Rubik/cube3x3/divers/mouvements.ml";;

(* matrices de transitions d'un angle à un autre *)

(* renvoie 0,1,2 suivant que l'arête de l'angle est parallèle au premier, deuxième ou troisième axe *)
let indice_angle v = let k = ref 0 in
		while v.(!k) <> 0 do incr k done;
		!k
;;

(* renvoie une matrice 'm' de rotation du cube différente de 'id' telle que 'vm=v', où 'v' est un angle *)
let stabilise_angle v =
	let i = indice_angle v in
		let j = (i + 1) mod 3 and k = (i + 2) mod 3 in
			let m = make_matrix 3 3 0 in
				m.(i).(i) <- - 1;
				m.(j).(k) <- v.(j) * v.(k);
				m.(k).(j) <- v.(j) * v.(k);
				m
;;

(* renvoie une matrice 'm' de rotation du cube telle que 'vm=w', avec 'm=id' si 'v=w', où 'v' et 'w' sont des angles *)
let trans_angle v w =
	let (i, i') = (indice_angle v, indice_angle w) in
		let j = (i + 1) mod 3 and k = (i + 2) mod 3
		and j' = (i' + 1) mod 3 and k' = (i' + 2) mod 3 in
			let a = v.(j) * w.(j') and b = v.(k) * w.(k') in
				let m = make_matrix 3 3 0 in
					m.(j).(j') <- a;
					m.(k).(k') <- b;
					m.(i).(i') <- a * b;
					m
;;

(* matrices de transition d'un coin à un autre *)

(* renvoie une matrice 'm' de rotation du cube différente de 'id' telle que 'vm=v', où 'v' est un coin *)
let stabilise_coin v =
	let (a, b, c) = vect v in
		let m = [|[|0; a * b; 0|]; [|0; 0; b * c|]; [|c * a; 0; 0|]|] in
			if a * b * c > 0 (* cohérence des sens de rotation *)
			then transpose m
			else m
;;

(* renvoie une matrice 'm' de rotation du cube telle que 'vm=w', avec 'm=id' si 'v=w' , où 'v' et 'w' sont des coins *)
let trans_coin v w =
	let (a, b, c) = (v.(0) * w.(0), v.(1) * w.(1), v.(2) * w.(2))
	and m = make_matrix 3 3 0 in
		m.(0).(0) <- a;
		if a * b * c = 1 then (
				m.(1).(1) <- b;
				m.(2).(2) <- c
			)
		else (
				m.(2).(1) <- v.(2) * w.(1);
				m.(1).(2) <- v.(1) * w.(2)
			);
		m
;;

let est_centre x = x /|/ x = 1;;
let est_angle x = x /|/ x = 2;;
let est_coin x = x /|/ x = 3;;


(* morphisme 's: M -> S' et section 'l: S -> M' *)
(* construction d'une section 'l' de la suite exacte '0 -> K -> M -> S -> 0' *)
(* En Caml on représente la sujection 's' par 'sur', la section 'l' par 'sec' et 'gij' par 'gg i j' *)

(* éléments g_{ij} alias gg i j de G servant à construire cette section *)
let gg i j =
	if est_angle i && est_angle j then trans_angle i j
	else if est_coin i && est_coin j then trans_coin i j
	else id
;;

(* stabilisateurs des angles et des coins *)
let st i = if est_angle i then stabilise_angle i
	else if est_coin i then stabilise_coin i
	else failwith "st"
;;

#use "exemples/OCaml/Rubik/cube3x3/divers/section_commun.ml";;
