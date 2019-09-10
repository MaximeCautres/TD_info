(* fournit le mouvement du cube 3x3 sous-jacent obtenu par appariement des angles... *)
let mv1_3_of_mv1_4 mv1_4 =
	let est_angle x = x /|/ x = 11
	in
		let encode a = [|a.(0) / 3; a.(1) / 3; a.(2) / 3|]
		and represente a = est_angle a && (a.(0) = 1 || a.(1) = 1 || a.(2) = 1) in
			map (fun (x, y) -> encode x, y) (select (fun (x, y) -> not (est_angle x) || represente x) mv1_4)
;;

(* ...en vue d'obtenir la rotation totale des angles correspondante *)
(* le paramètre mv1_4 est ici le mouvement de type mv1 du cube 4x4x4 *)
let rta mv1_4 =
	let (_, _, ker, st) = psd_of_mv1_3 ()
	and est_angle x = x /|/ x = 2
	and mv1_3 = mv1_3_of_mv1_4 mv1_4
	and fun_of_mv1 mv1 i = assoc i mv1
	in
		let rta_aux k = let f = fun_of_mv1 k in
				let ea i = if f i = st i then 1 else 0 in
					let angles = select est_angle (map fst k) in
						(list_it (+) (map ea angles) 0) mod 2
		in rta_aux (ker mv1_3)
;;


(* signature de la permutation des angles d'un mouvement adapté au sous-cube 3x3... *)
let sign_angles mv1 =
	let encode a = [|a.(0) / 3; a.(1) / 3; a.(2) / 3|]
	and decode a = let f x = if x = 0 then 1 else if x = 1 then 3 else - 3 in map_vect f a
	and represente a =
		let est_angle x = (x /|/ x) = 19 in
			est_angle a && (a.(0) = 1 || a.(1) = 1 || a.(2) = 1)
	in
		let perm x = encode (sur mv1 (decode x))
		and angles = map encode (select represente (map fst mv1))
		in
			sign angles perm
;;

(* ...en vue de corriger ce cube : la fonction fix_cube est destinée au troisième niveau du cube 3x3 *)
(* à n'utiliser que si le cube 3x3 issu du 4x4 n'a pas été déjà corrigé *)
let fix_cube cube =
	let pos1 x =
		let pp = cube.context1.matrice
		in
			transpose pp /./ (fun_of_mv1 (inverse cube.mouvement1.mv1)) (x /:/ transpose pp) /./ pp
	and (OPS (a0, _, _), OPS (a0', _, _)) = cube.op_globales1
	and (OPS (_, di, hi), OPS (_, di', _), OPS (_, gi, _), OPS (_, gi', _)) = cube.op_externes1i
	and (OPS (a, _, h), OPS (_, _, _), OPS (p, _, _), OPS (_, _, _)) = cube.op_externes1
	in
		let test_cube cube =
			let est_mal_oriente angle =
				let jumeau a =
					let f x = if x = 1 then - 1 else if x = - 1 then 1 else x in map_vect f a
				
				in
					let m = pos1 angle and m' = pos1 (jumeau angle) in
						m.(2) <> [|0; 0; 1|] && m'.(2) <> [|0; 0; 1|]
			in
				let v = map_vect est_mal_oriente
					[|[|3; 1; - 3|]; [|1; - 3; - 3|]; [|- 3; 1; - 3|]; [|1; 3; - 3|]|] in
					let coins = select (fun t -> t /|/ t = 27) indices
					in
						let n = list_it (fun a b -> if a = false then b + 1 else b) (list_of_vect v) 0 in
							(n mod 2), (sign_angles cube.mouvement1.mv1), (sign coins (sur (cube.mouvement1.mv1)))
		in
			let fix_flip () = exe [a0; a0; di; di; p; p; h; h; gi; h; h; di'; h; h; di; h; h; a; a; di; a; a; gi'; p; p; di; di; a0; a0]
			and fix_parity () = exe [a0; a0; di; di; h; h; di; di; hi; hi; h; h; di; di; hi; hi; a0; a0]
			in
				let (n, sa, sc) = test_cube cube
				in
					if sa <> sc then fix_parity ();
					if n mod 2 <> 0 then fix_flip ();
;;


(*- vérifications préalables au traitement du cube 3x3 sous-jacent -*)

(* s'assure que la rotation totale des angles du cube 3x3 sous-jacent est nulle. *)
(* suppose qu'on ait déjà apparié les angles à l'aide de la fonction 'apparier' *)
let verifier_angles cube =
	let (OPS (a0, _, _), OPS (_, _, _)) = cube.op_globales1
	and (OPS (_, di, _), OPS (_, di', _), OPS (_, gi, _), OPS (_, gi', _)) = cube.op_externes1i
	and (OPS (a, _, h), OPS (_, _, _), OPS (p, _, _), OPS (_, _, _)) = cube.op_externes1
	in
		if rta cube.mouvement1.mv1 <> 0 then
			exe [a0; a0; di; di; p; p; h; h; gi; h; h; di'; h; h; di; h; h; a; a; di; a; a; gi'; p; p; di; di; a0; a0];
;;

(* s'assure que les permutations d'angles et de coins du cube 3x3 sous-jacent sont égales *)
(* suppose qu'on ait déjà  apparié les angles à l'aide de la fonction 'apparier' *)
let verifier_parites cube =
	let (OPS (a0, _, _), OPS (_, _, _)) = cube.op_globales1
	and (OPS (_, di, hi), OPS (_, _, _), OPS (_, _, _), OPS (_, _, _)) = cube.op_externes1i
	and (OPS (a, _, h), OPS (_, _, _), OPS (_, _, _), OPS (_, _, _)) = cube.op_externes1
	in
		let sign_coins mv1 = sign (select est_coin indices) (sur mv1)
		in
			if sign_angles cube.mouvement1.mv1 <> sign_coins cube.mouvement1.mv1 then
				exe [a0; a0; di; di; h; h; di; di; hi; hi; h; h; di; di; hi; hi; a0; a0];
			if sign_coins cube.mouvement1.mv1 mod 2 = - 1 then exe [a]; (* permutations de coins et d'angles paires *)
;;

(*- fin de vérifications préalables au traitement du cube 3x3 sous-jacent -*)


(*- nombres de quarts de tour des tranches externes modulo 4 -*)
let nqt1 mv1 x =
	let r = fun_of_mv1 mv1 x
	and encode x = [|x.(0) / 3; x.(1) / 3; x.(2) / 3|]
	in
		let v = encode x in
			if r = id then 0
			else if r = rot v then 1
			else if r = rot v /./ rot v then 2
			else if r = rot' v then 3
			else failwith "nqt"
;;

let nqt cube v =
	let p = cube.context1.matrice in
		let w = [|v.(0) * 3; v.(1) * 3; v.(2) * 3|] in
			nqt1 cube.mouvement1.mv1 (w /:/ transpose p)
;;

(*- fin de nombres de quarts de tour des tranches externes modulo 4 -*)


(* résolution par niveaux du cube 3x3 sous-jacent au 4x4 *)
(* ce cube 3x3 est obtenu en utilisant les fonctions 'tous_les_centres cube' et 'apparier cube' *)
(* etc... définies dans le fichier principal 'cube4x4.oml' *)

exception Orienter_les_coins;;
exception Placer_angle_frontal_haut;;
exception Descendre_coin;;
exception Remonter_coin;;
exception Remonter_angle;;
exception Orienter_les_angles;;
exception Placer_les_angles;;
exception Placer_les_coins;;

let resoudre_le_cube3x3 cube tester completement =
	(* passer tester = true si le cube 3x3 n'a pas été testé lors de son assemblage, tester = false sinon *)
	(* si tester = true, le test 'fix_cube' est fait au début du traitement du troisième niveau *)
	(* '(y, m) = pos0 x' : le minicube d'indice 'x' est à l'emplacement d'indice 'y' et 'm' est sa matrice *)
	(* de déplacement (telle que 'y=xm') (repère ADH) *)
	(* '(x, m) = pos1 y' : l'emplacement d'indice 'y' est occupé par le minicube d'indice 'x' et 'm' est sa matrice *)
	(* de déplacement inverse (telle que 'x = my') (repère ADH) *)
	
	let encode x = [|x.(0) / 3; x.(1) / 3; x.(2) / 3|]
	and decode x = let f t = if t = 0 then 1 else if t > 0 then 3 else - 3
		in map_vect f x
	in
		
		let pos0, pos1 =
			let pos mv1 t =
				let x = decode t in
					let p = cube.context1.matrice in
						let m = transpose p /./ (fun_of_mv1 mv1) (x /:/ transpose p) /./ p
						in
							encode (x /:/ m), m
			in
				(fun x -> pos cube.mouvement1.mv1 x),
				(fun x -> pos (inverse cube.mouvement1.mv1) x)
		
		
		and (OPS (_, _, h0), OPS (_, _, h0')) = cube.op_globales1
		and (OPS (a, d, h), OPS (a', d', h'), OPS (p, g, b), OPS (p', g', b')) = cube.op_externes1 (*tranches externes*)
		in
			
			let niveau_superieur () =
				
				(* niveau supérieur *)
				
				let orienter_le_centre () =
					let n = nqt cube ([|0; 0; 1|]) in
						if n = 1 then exe [h']
						else if n = 2 then exe [h; h]
						else if n = 3 then exe [h]
				
				and placer_et_orienter_les_angles () =
					let placer_angle_frontal_haut () =
						let v, _ = pos0 [|1; 0; 1|] in
							match vect v with
								| (1, 0, 1) -> ()
								| (1, 1, 0) -> exe [a']
								| (1, 0, - 1) -> exe [a; a]
								| (1, - 1, 0) -> exe [a]
								| (0, 1, 1) -> exe [d'; a']
								| (- 1, 1, 0) -> exe [h'; d'; h]
								| (0, 1, - 1) -> exe [d; a'; d']
								| (0, - 1, 1) -> exe [g; a]
								| (- 1, - 1, 0) -> exe [h; g; h']
								| (0, - 1, - 1) -> exe [g'; a; g]
								| (- 1, 0, 1) -> exe [p; p; b; b; a; a]
								| (- 1, 0, - 1) -> exe [b; b; a; a]
								| _ -> raise Placer_angle_frontal_haut
					and mal_oriente () =
						(snd (pos0 [|1; 0; 1|])).(2) <> [|0; 0; 1|]
					in
						for i = 0 to 3 do
							placer_angle_frontal_haut ();
							if mal_oriente () then exe [h'; d'; h; a'];
							exe [h0]
						done
				
				and placer_et_orienter_les_coins () =
					let descendre_coin () =
						let w, m = pos0 [|1; 1; 1|] in
							if (w = [|1; 1; 1|]) && (m = id) then ()
							else
								match vect w with
									| (- 1, 1, 1) -> exe [p'; b'; p]
									| (- 1, - 1, 1) -> exe [p; b; b; p']
									| (1, - 1, 1) -> exe [g; b; g']
									| (1, 1, 1) -> exe [a; b; a'; b']
									| (- 1, 1, - 1) -> exe [b']
									| (- 1, - 1, - 1) -> exe [b; b]
									| (1, - 1, - 1) -> exe [b]
									| (1, 1, - 1) -> ()
									| _ -> raise Descendre_coin
					and remonter_coin () =
						let (v, m) = pos0 [|1; 1; 1|] in
							if (v = [|1; 1; 1|]) && (m = id) then ()
							else
								let w = m.(2) in match vect w with
										| (1, 0, 0) -> exe [d; a'; d'; a]
										| (0, 1, 0) -> exe [a'; d; a; d']
										| (0, 0, - 1) -> exe [a; b'; a'; b; b; d; a'; d'; a]
										| _ -> raise Remonter_coin
					in
						for i = 0 to 3 do
							descendre_coin ();
							remonter_coin ();
							exe [h0]
						done;
				
				in
					if completement then orienter_le_centre ();
					placer_et_orienter_les_angles ();
					placer_et_orienter_les_coins ()
			
			and niveau_median () =
				
				(* niveau médian *)
				
				let orienter_les_centres_lateraux () =
					let aux () =
						let n = nqt cube [|1; 0; 0|]
						in
							if n <> 0 then
								(
									exe [a; a; b; b];
									(
										if n = 1 then
											exe [a']
										else if n = 2 then
											exe [a; a]
										else if n = 3 then
											exe [a];
									);
									exe [b; b; a; a]
								)
					in
						exe [aux; h0; aux; h0; aux; h0; aux; h0]
				
				and placer_angle_frontal_droit () =
					let descendre_angle () =
						let aux () = exe [b; a; b'; a'; b'; d'; b; d] in
							let x, _ = pos0 [|1; 1; 0|] in
								match vect x with
									| (1, 1, 0) -> aux ()
									| (- 1, 1, 0) -> exe [h0; aux; h0']
									| (- 1, - 1, 0) -> exe [h0; h0; aux; h0; h0]
									| (1, - 1, 0) -> exe [h0'; aux; h0]
									| _ -> ()
					and remonter_angle () =
						let aux_r () = exe [b'; d'; b; d; b; a; b'; a']
						and aux_l () = exe [b; a; b'; a'; b'; d'; b; d]
						in
							let x, m = pos0 [|1; 1; 0|] in
								if m.(0) <> [|0; 0; - 1|] then
									match vect x with
										| (1, 0, - 1) -> aux_r ()
										| (0, - 1, - 1) -> exe [b; aux_r]
										| (- 1, 0, - 1) -> exe [b; b; aux_r]
										| (0, 1, - 1) -> exe [b'; aux_r]
										| _ -> raise Remonter_angle
								else
									match vect x with
										| (1, 0, - 1) -> exe [b; aux_l]
										| (0, - 1, - 1) -> exe [b; b; aux_l]
										| (- 1, 0, - 1) -> exe [b'; aux_l]
										| (0, 1, - 1) -> aux_l ()
										| _ -> raise Remonter_angle
					in
						let x, m = pos0 [|1; 1; 0|] in
							if (x = [|1; 1; 0|]) && (m = id) then ()
							else (
									descendre_angle ();
									remonter_angle ()
								)
				
				in
					if completement then orienter_les_centres_lateraux ();
					for i = 0 to 3 do
						placer_angle_frontal_droit ();
						exe [h0]
					done
			
			and niveau_inferieur () =
				
				(* niveau inférieur *)
				
				let orienter_les_angles () =
					
					let est_mal_oriente angle =
						let (_, m) = pos1 angle in
							m.(2) <> [|0; 0; 1|]
					in
						let v = map_vect est_mal_oriente
							[|[|1; 0; - 1|]; [|0; - 1; - 1|]; [|- 1; 0; - 1|]; [|0; 1; - 1|]|]
						in match (v.(0), v.(1), v.(2), v.(3)) with
								| (false, false, false, false) -> ()
								| (true, true, true, true) -> exe [d; b; a; b'; a'; d'; b; d; a; b; a'; b'; d'];
								
								| (false, false, true, true) -> exe [h0; d; b; a; b'; a'; d']
								| (true, false, false, true) -> exe [d; b; a; b'; a'; d']
								| (true, true, false, false) -> exe [h0'; d; b; a; b'; a'; d']
								| (false, true, true, false) -> exe [h0; h0; d; b; a; b'; a'; d']
								
								| (false, _, false, _) -> exe [d; a; b; a'; b'; d']
								| (_, false, _, false) -> exe [h0; d; a; b; a'; b'; d']
								
								| _ -> raise Orienter_les_angles
				
				and placer_les_angles () =
					let permuter () =
						(* laisse fixe l'angle arrière et permute circulairement les autres *)
						(* dans le sens direct vu D'en bas *)
						exe [d; b; b; d'; b'; d; b'; d']
					and permuter' () =
						(* laisse fixe l'angle arrière et permute circulairement les autres *)
						(* dans le sens indirect vu D'en bas *)
						exe [d; b; d'; b; d; b; b; d']
					in
						
						let chercher_un_angle_bien_place () =
							let i = ref 0 in
								while !i < 4 && fst (pos0 [|- 1; 0; - 1|]) <> [|- 1; 0; - 1|] do
									exe [h0];
									incr i
								done;
								!i
						in
						(* la méthode utilisée suppose ici que la permutation des angles soit paire *)
							if completement then (* maintenir la somme des rotations des milieux nulle modulo 4, *)
							(*ce qui implique une permutation paire des coins donc des angles du cube 3x3 *)
								(let n = nqt cube ([|0; 0; - 1|]) in
										if n = 1 then exe [b'] else if n = 3 then exe [b] else if n = 2 then exe [b; b]
								)
							else (* maintenir une permutation paire des angles donc des coins *)
								(
									if sign_angles cube.mouvement1.mv1 = - 1 then exe [b]
									else ())
							;
							let j = chercher_un_angle_bien_place () in
								if j = 4 (* aucun angle bien placé *) then (
										permuter ();
										let _ = chercher_un_angle_bien_place () in ()
									)
								else ();
								
								let v, _ = pos0 [|1; 0; - 1|] in match vect v with
										| (0, - 1, - 1) -> permuter ()
										| (0, 1, - 1) -> permuter' ()
										| (1, 0, - 1) -> ()
										| _ -> raise Placer_les_angles
				
				and placer_les_coins () =
					(* à ce stade la permutation des coins devrait être paire *)
					let permuter () =
						(* laisse fixe le coin frontal droit et permute circulairement 
          les autres dans le sens direct vu d'en bas *)
						exe [b; a; b'; p'; b; a'; b'; p]
					and permuter' () =
						(* laisse fixe le coin frontal droit et permute circulairement 
          les autres dans le sens indirect vu d'en bas *)
						exe [p'; b; a; b'; p; b; a'; b']
					in
						let chercher_un_coin_bien_place () =
							let i = ref 0 in
								while !i < 4 && fst (pos0 [|1; 1; - 1|]) <> [|1; 1; - 1|] do
									exe [h0];
									incr i
								done;
								!i
						in
							let j = chercher_un_coin_bien_place () in
								
								if j = 4 (* aucun coin bien placé *) then (
										permuter ();
										let _ = chercher_un_coin_bien_place () in ()
									)
								else ();
								
								let v, _ = pos0 [|- 1; - 1; - 1|] in match vect v with
										| (1, - 1, - 1) -> permuter ()
										| (- 1, 1, - 1) -> permuter' ()
										| (- 1, - 1, - 1) -> ()
										| _ -> raise Placer_les_coins
				
				
				and orienter_les_coins () =
					let faire_tourner () =
						(* fait tourner les coins frontaux inférieurs sur eux-mêmes: 
          le coin gauche dans le sens direct, le coin droit en sens inverse *)
						exe [p'; b'; p; b'; p'; b; b; p];
						exe [a; b; a'; b; a; b; b; a']
					and
					faire_tourner' () =
						(* fait tourner les coins frontaux inférieurs sur eux-mêmes:
          le coin droit dans le sens direct, le coin gauche en sens inverse *)
						exe [a; b; b; a'; b'; a; b'; a'];
						exe [p'; b'; b'; p; b; p'; b; p]
					in
						let orienter_frontal_inferieur_droit () =
							let _, m = pos0 [|1; 1; - 1|] in
								let v = m.(2) in
									match vect v with
										| (0, 0, 1) -> ()
										| (- 1, 0, 0) -> faire_tourner' ()
										| (0, - 1, 0) -> faire_tourner ()
										| _ -> raise Orienter_les_coins
						in
							for i = 0 to 2 do
								orienter_frontal_inferieur_droit ();
								exe [h0']
							done
				in
					orienter_les_angles ();
					placer_les_angles ();
					placer_les_coins ();
					orienter_les_coins ();
			
			in
				if completement then
				(* les centres formant les milieux du cube 3x3 sont censés être à leurs places après appel de la fonction 'arranger les centres' *)
				(* éviter maintenant la rotation de ces centres *)
					cube.mouvement1.mv1 <- map (fun (x, m) -> x, if not (est_centre x || est_angle x || est_coin x) then id else m) cube.mouvement1.mv1;
				let ctx = cube.context1.matrice in
					try
						(
							let n = list_length !(cube.liste_ops1) in
								(if (cube.liste_mouvements) then (
											printf__printf "\nNOMBRE D'OPÉRATIONS PRÉALABLES : %d\n" n;
											print_newline ();
										);
									niveau_superieur ();
									if cube.liste_mouvements then (
											printf__printf "\nNIVEAU SUPÉRIEUR : %d quarts de tour\n" (list_length !(cube.liste_ops1) - n);
											print_newline ()
										)
								)
						);
						(
							let n = list_length !(cube.liste_ops1) in
								(
									niveau_median ();
									if cube.liste_mouvements then (
											printf__printf "\nNIVEAU MÉDIAN : %d quarts de tour\n" (list_length !(cube.liste_ops1) - n);
											print_newline ()
										)
								)
						);
						if tester then
						(* test à faire si le cube 3x3 n'a pas été testé lors de l'assemblage de ses angles et de ses milieux *)
							(
								let n = list_length !(cube.liste_ops1) in
									(
										fix_cube cube;
										if cube.liste_mouvements then (
												printf__printf "\nTRAITEMENT DES PARITÉS : %d\n" (list_length !(cube.liste_ops1) - n);
												print_newline ()
											)
									)
							);
						(
							let n = list_length !(cube.liste_ops1) in
								(
									niveau_inferieur ();
									if cube.liste_mouvements then (
											printf__printf "\nNIVEAU INFÉRIEUR : %d quarts de tour" (list_length !(cube.liste_ops1) - n);
											print_newline ()
										)
								)
						);
						cube.context1.matrice <- ctx;
						cube.dessine1 ();
						!(cube.liste_ops1)
					with
						| Orienter_les_coins ->
									print_string "orienter_les_coins\n"; !(cube.liste_ops1)
						| Placer_les_coins ->
									print_string "placer_les_coins\n"; !(cube.liste_ops1)
						| Placer_les_angles ->
									print_string "placer_les_angles\n"; !(cube.liste_ops1)
						| Orienter_les_angles ->
									print_string "orienter_les_angles\n"; !(cube.liste_ops1)
;;
