include "exemples/Caml Light/Rubik/cube4x4/divers/types.ml";;
include "exemples/Caml Light/Rubik/divers/divers.ml";;
include "exemples/Caml Light/Rubik/cube4x4/divers/section_marques.ml";;
include "exemples/Caml Light/Rubik/cube4x4/divers/cube3x3_marques.ml";;
include "exemples/Caml Light/Rubik/divers/couleurs.ml";;
include "exemples/Caml Light/Rubik/cube4x4/divers/graphisme.ml";;
include "exemples/Caml Light/Rubik/cube4x4/divers/boutons.ml";;

(* mouvement général de type 'mv1' défini par les rotations de coins *)
(* et les permutations des centres (ou 'milieux'), des angles et des coins *)
let nouveau_mv1 pm pa pc ec =
	let k = mv1_of_fun
		(fun i ->
							if est_coin i then
								if ec i = 0 then id else if ec i = 1 then st i
								else transpose (st i)
							else id
		)
	and l = mv1_of_fun
		(
			fun i ->
							if est_angle i then
								gg i (pa i)
							else if est_coin i then gg i (pc i)
							else if est_centre i then gg i (pm i)
							else id
		)
	in k /*/ l
;;

let angles = select est_angle indices;;
let coins = select est_coin indices;;
let centres = select est_centre indices;;


(*- mouvement général aléatoire de type mv1 -*)

random__init (unix__time ());;

(* permutation aléatoire d'une liste *)
let pl_r l =
	let l' = random_list l
	in fun i -> assoc i (map2 (fun x y -> x, y) l l')
;;

(* exposant aléatoire pour les coins *)
let ec_r = fun i -> if est_coin i then random__int 3 else failwith "ec_r";;

(* mouvement aléatoire général *)
let mv1_r () = nouveau_mv1 (pl_r centres) (pl_r angles) (pl_r coins) ec_r;;

(*- fin de mouvement général aléatoire de type mv1 -*)


(*- rotation totale des coins et test d'appartenance d'un mouvement au sous-groupe de Rubik R -*)

(* rotation totale des coins *)
let rtc m =
	let rtc_aux k = let f = fun_of_mv1 k in
			let indexc i = if f i = st i then 1
				else if f i = transpose (st i) then 2 else 0 in
				(list_it (prefix +) (map indexc coins) 0) mod 3
	in rtc_aux (ker m)
;;

(* Sous groupe de Rubik R de M *)

(* test d'appartenance d'un mouvement au sous-groupe R *)
(* par nullité de la rotation totale des coins et égalité *)
(* des signatures des permutations des centres et des coins *)
(* voir aussi plus loin la fonction 'est_rubik' *)
let est_dans_R m = let p = sur m in
		sign centres p = sign coins p && rtc m = 0;;

(* mouvement de Rubik aléatoire *)
let mv1_rubik_r () =
	let rot_coin i n =
		nouveau_mv1 (fun x -> x) (fun x -> x) (fun x -> x) (fun j -> if j = i then n else 0)
	in
		let m = ref (mv1_r ()) in
			if rtc !m <> 0 then m := !m /*/ rot_coin [|3; 3; 3|] (3 - rtc !m);
			let p = sur !m in
				if sign coins p <> sign centres p then
					(
						let tr i j = sec (fun k -> if k = i then j else if k = j then i else k)
							(* mouvement de transposition de deux indices... *)
						in
							m := !m /*/ tr [|3; 3; 3|] [|3; - 3; 3|]
							(* ...appliqué à deux coins pour s'assurer que les permutations *)
							(* des coins et des centres ont même signature *)
					);
				!m
;;

(*- fin de rotations totales et test d'appartenance d'un mouvement au sous-groupe de Rubik R -*)


(*- initialisation d'un cube 4x4 : mise en place des mouvements élémentaires de Rubik -*)

let nouveau_cube mouvement context dessine liste_mouvements =
	
	let listeops = ref []
	and dessine () = dessine context mouvement.mv1
	in
		let op_externesI liste_ops =
			let fct x () =
				let t = x /:/ transpose context.matrice in
					mouvement.mv1 <- mouvement.mv1 /*/ rub t;
					if liste_mouvements then (
							print_string (nom_de_face t ^ "I ");
							liste_ops := !liste_ops @ [nom_de_face t ^ "I"];
						);
					dessine ()
			and fct' x () =
				let t = x /:/ transpose context.matrice in
					mouvement.mv1 <- mouvement.mv1 /*/ rub' t;
					if liste_mouvements then (
							print_string (nom_de_face t ^ "I' ");
							liste_ops := !liste_ops @ [nom_de_face t ^ "I'"];
						);
					dessine ()
			in
				let (a, d, h) = vect (map_vect fct id)
				and (a', d', h') = vect (map_vect fct' id)
				and (p, g, b) = vect (map_vect fct idm)
				and (p', g', b') = vect (map_vect fct' idm)
				in (OPS (a, d, h), OPS (a', d', h'), OPS (p, g, b), OPS (p', g', b'))
		
		and op_externes liste_ops =
			let fct x () =
				let t = x /:/ transpose context.matrice in
					mouvement.mv1 <- mouvement.mv1 /*/ rub3 t;
					if liste_mouvements then (
							print_string (nom_de_face t ^ " ");
							liste_ops := !liste_ops @ [nom_de_face t];
						);
					dessine ()
			and fct' x () =
				let t = x /:/ transpose context.matrice in
					mouvement.mv1 <- mouvement.mv1 /*/ rub3' t;
					if liste_mouvements then (
							print_string (nom_de_face t ^ "' ");
							liste_ops := !liste_ops @ [nom_de_face t ^ "'"];
						);
					dessine ()
			in
				let (a, d, h) = vect (map_vect fct id)
				and (a', d', h') = vect (map_vect fct' id)
				and (p, g, b) = vect (map_vect fct idm)
				and (p', g', b') = vect (map_vect fct' idm)
				in (OPS (a, d, h), OPS (a', d', h'), OPS (p, g, b), OPS (p', g', b'))
		
		and op_internesI () =
			let fct x () =
				mouvement.mv1 <- mouvement.mv1 /*/ rub x;
				if liste_mouvements then print_string (nom_de_face x ^ "I ");
				dessine ()
			and fct' x () =
				mouvement.mv1 <- mouvement.mv1 /*/ rub' x;
				if liste_mouvements then print_string (nom_de_face x ^ "I' ");
				dessine ()
			in
				let (o, v, blanc) = vect (map_vect fct id)
				and (o', v', blanc') = vect (map_vect fct' id)
				and (r, b, j) = vect (map_vect fct idm)
				and (r', b', j') = vect (map_vect fct' idm)
				in (OPS (o, v, blanc), OPS (o', v', blanc'), OPS (r, b, j), OPS (r', b', j'))
		
		and op_internes () =
			let fct x () =
				mouvement.mv1 <- mouvement.mv1 /*/ rub3 x;
				if liste_mouvements then print_string (nom_de_face x ^ " ");
				dessine ()
			and fct' x () =
				mouvement.mv1 <- mouvement.mv1 /*/ rub3' x;
				if liste_mouvements then print_string (nom_de_face x ^ "' ");
				dessine ()
			in
				let (o, v, blanc) = vect (map_vect fct id)
				and (o', v', blanc') = vect (map_vect fct' id)
				and (r, b, j) = vect (map_vect fct idm)
				and (r', b', j') = vect (map_vect fct' idm)
				in (OPS (o, v, blanc), OPS (o', v', blanc'), OPS (r, b, j), OPS (r', b', j'))
		
		and op_globales () =
			let rotate pp () = context.matrice <- context.matrice /./ pp;
				dessine () in
				let (a, d, h) = vect (map_vect rotate (map_vect rot id))
				and (a', d', h') = vect (map_vect rotate (map_vect rot' id))
				in
					(OPS (a, d, h), OPS (a', d', h'))
		
		in
			let op_ext = op_externes listeops and op_extI = op_externesI listeops and op_int = op_internes () and op_intI = op_internesI () in
				let op_from_strings liste_ops =
					let (OPS (orangeI, vertI, blancI), OPS (orangeI', vertI', blancI'), OPS (rougeI,
					bleuI, jauneI), OPS (rougeI', bleuI', jauneI')) = op_intI
					and (OPS (orange, vert, blanc), OPS (orange', vert', blanc'), OPS (rouge,
					bleu, jaune), OPS (rouge', bleu', jaune')) = op_int
					
					in
						let aux s = assoc s
							[("orangeI", orangeI); ("vertI", vertI); ("blancI", blancI);
								("orangeI'", orangeI'); ("vertI'", vertI'); ("blancI'", blancI');
								("rougeI", rougeI); ("bleuI", bleuI); ("jauneI", jauneI);
								("rougeI'", rougeI'); ("bleuI'", bleuI'); ("jauneI'", jauneI');
								("orange", orange); ("vert", vert); ("blanc", blanc);
								("orange'", orange'); ("vert'", vert'); ("blanc'", blanc');
								("rouge", rouge); ("bleu", bleu); ("jaune", jaune);
								("rouge'", rouge'); ("bleu'", bleu'); ("jaune'", jaune')]
						in
							let rec op_from_strings_aux liste_ops =
								match liste_ops with
									| t :: r -> aux t :: op_from_strings_aux r
									| [] -> []
							in op_from_strings_aux liste_ops
				in
					{mouvement1 = mouvement; context1 = context; dessine1 = dessine;
						liste_mouvements = liste_mouvements;
						op_globales1 = op_globales ();
						op_externes1 = op_ext;
						op_externes1i = op_extI;
						op_internes1 = op_int;
						liste_ops1 = listeops;
						op_internes1i = op_intI;
						op_from_strings1 = op_from_strings;
						boutons1 = make_vect 1 {titre = ""; orx = 0; ory = 0; largeur = 0;
							hauteur = 0; couleur = 0; action = fun () -> ()}
					}
;;

(* mélanger le cube par une suite aléatoire de mouvements de Rubik élémentaires *)
let melanger cube =
	let f = fun (OPS (a, b, c)) -> [a; b; c]
	and (ops1, ops2, ops3, ops4) = cube.op_externes1
	and (ops5, ops6, ops7, ops8) = cube.op_externes1i
	and t = make_vect 50 (fun () -> ())
	in
		(
			let v = vect_of_list (flat_map f [ops1; ops2; ops3; ops4; ops5; ops6; ops7; ops8])
			in
				for i = 0 to 49 do
					t.(i) <- v.(random__int 24);
				done;
				exe (list_of_vect t)
		);
		cube.liste_ops1 := []
;;

(* cube muet sans affichage graphique dans l'état 'mv1' avec orientation standard *)
let nouveau_cube_muet mv1 =
	nouveau_cube mv1 {matrice = id} (fun _ _ -> ()) false
;;

(* cube verbeux sans affichage graphique dans l'état 'mv1' avec orientation standard *)
let nouveau_cube_verbeux mv1 =
	nouveau_cube mv1 {matrice = id} (fun _ _ -> ()) true
;;

(* cube verbeux avec affichage graphique dans l'état 'mv1' avec orientation standard *)
let nouveau_cube_graphique mv1 =
	nouveau_cube mv1 {matrice = id} dessine_cube true;;

(*- fin de initialisation d'un cube 4x4 : mise en place des mouvements élémentaires de Rubik -*)


(*- fonctions retournant les couleurs des faces des minicubes dans l'état actuel du cube -*)
(* paramètres en repère ADH *)

(* couleur présentée dans la face normale au vecteur sortant 'v' par le minicube centré en 'i' *)
let couleur cube v i =
	let p = transpose cube.context1.matrice
	and mv1 = inverse (cube.mouvement1.mv1)
	in
		nom_de_face (v /:/ p /:/ (fun_of_mv1 mv1) (i /:/ p))
;;

(* dans cette fonction 'c' est censé être l'indice d'un minicube central *)
(* couleur présentée par le minicube central centré en 'c' : repère ADH *)
let couleur_centre cube c = couleur cube [|c.(0) / 3; c.(1) / 3; c.(2) / 3|] c;;

(* couleur de la face orthogonale au vecteur sortant 'v' : repère ADH *)
(* dans la position actuelle du cube : cette couleur dépend de cube.context1.matrice *)
let couleur_face cube v =
	nom_de_face (v /:/ transpose cube.context1.matrice)
;;

(*- fin de fonctions retournant les couleurs des faces des minicubes dans l'état actuel du cube -*)


(*- étapes pour la résolution par niveaux du Rubik's cube 4x4 -*)

(*-- regrouper les centres par couleurs --*)

(* liste des positions ADH des centres restant à placer sur la face haute *)
let centres_restants cube =
	select (fun c -> couleur_centre cube c = couleur_face cube [|0; 0; 1|] && c.(2) <> 3)
	(select est_centre (map fst cube.mouvement1.mv1))
;;

(* placer un centre de la face haute *)
(* en ne transitant que par la face antérieure *)
exception Placer_centre;;

let placer_centre cube c =
	let (OPS (_, d0, h0), OPS (_, d0', h0')) = cube.op_globales1
	and (OPS (ai, _, _), OPS (ai', _, _), OPS (_, _, bi), OPS (_, _, bi')) = cube.op_externes1i
	and (OPS (a, _, h), OPS (a', _, h'), OPS (_, _, _), OPS (_, _, _)) = cube.op_externes1
	in let l = [ai; bi; ai'; bi'; h'; bi; ai; bi'; ai'; h]
		in
			let placement final c1 () =
				(
					if final then
						(
							let i = hd (select (fun x -> couleur_centre cube x <> couleur_face cube [|0; 0; 1|])
									(select (fun x -> est_centre x && x.(2) = 3) (map fst cube.mouvement1.mv1)))
							in (match vect i with
										| (1, - 1, 3) -> ()
										| (- 1, - 1, 3) -> exe [h']
										| (- 1, 1, 3) -> exe [h; h]
										| (1, 1, 3) -> exe [h]
										| _ -> ()
								)
						);
					
					(* c1 demande : qui suis-je dans ce nouveau contexte ?... réponse: c *)
					let c = c1 /:/ cube.context1.matrice in
						match vect c with
							| (3, - 1, - 1) -> ()
							| (3, - 1, 1) -> exe [a']
							| (3, 1, 1) -> exe [a; a];
							| (3, 1, - 1) -> exe [a]
							| _ -> ()
				);
				exe l
			in (* c demande: dans le contexte actuel, qui suis-je réellement ?... réponse: c1 *)
				let c1 = c /:/ transpose (cube.context1.matrice) in
					match vect c with
						| - 3, _, _ -> exe [h0; h0; placement true c1; h0; h0]
						| 3, _, _ -> exe [placement true c1]
						| _, - 3, _ -> exe [h0'; placement true c1; h0]
						| _, 3, _ -> exe [h0; placement true c1; h0']
						| _, _, - 3 -> exe [d0; placement false c1; d0']
						| _ -> raise Placer_centre
;;

(* placer les centres de la face haute *)
let centre_haut cube =
	try
		while true do
			let c = hd (centres_restants cube) in
				placer_centre cube c
		done
	with Failure "hd" -> ()
;;

(* chaque centre rejoint la face à laquelle sa couleur visible est attribuée : *)
(* en basculant le cube chaque face est amenée en haut avant d'appeler 'centre_haut'*)
let regrouper_les_centres cube =
	let (OPS (a0, d0, _), OPS (_, d0', _)) = cube.op_globales1
	and u = fun () -> centre_haut cube;
	in
		exe [u; d0; u; a0; u; a0; u; a0; u; a0; u; d0'];
;;

(*-- fin de regrouper les centres par couleurs --*)


(*-- arranger les centres : seulement en vue d'une résolution complète du cube --*)

(* fait en sorte que chaque centre soit à sa place dans sa face. *)
(* sachant qu'on a déjà amené chaque centre dans la face ayant même *)
(* couleur que lui à l'aide de la fonction 'regrouper_les_centres' *)
exception Arranger_les_centres;;

let arranger_les_centres cube =
	let (OPS (a0, d0, h0), OPS (_, d0', _)) = cube.op_globales1
	and (OPS (ai, _, _), OPS (ai', _, _), OPS (pi, _, bi), OPS (pi', _, bi')) = cube.op_externes1i
	and (OPS (_, _, h), OPS (_, _, h'), OPS (_, _, _), OPS (_, _, _)) = cube.op_externes1
	and pos0, pos1 =
		let pos cube mv1 x =
			let pp = cube.context1.matrice in
				let m = transpose pp /./ (fun_of_mv1 mv1) (x /:/ transpose pp) /./ pp
				in
					x /:/ m, m
		in
			(fun cube x -> pos cube cube.mouvement1.mv1 x),
			(fun cube x -> pos cube (inverse cube.mouvement1.mv1) x)
	and centres_dans_face cube i =
		let encode x = [|x.(0) / 3; x.(1) / 3; x.(2) / 3|] in
			let est_dans_face cube i x = i /:/ transpose cube.context1.matrice = encode x
			in select (est_dans_face cube i) centres
	in
		let ah () =
			let ll = [ai; bi; ai'; bi'; h'; bi; ai; bi'; ai'; h]
			and ll' = [h'; ai; bi; ai'; bi'; h; bi; ai; bi'; ai']
			in
				let m () = exe ll
				and m' () = exe ll'
				in (* faire en sorte d'abord que la permutations des centres hauts soit paire *)
					if sign (centres_dans_face cube [|0; 0; 1|]) (sur cube.mouvement1.mv1) = - 1 then exe [h];
					let faire_le_tour () =
						let i = ref 0 in
							while !i < 4 && fst (pos0 cube [|- 1; 1; 3|]) <> [|- 1; 1; 3|] do
								exe [h0];
								incr i
							done;
							if !i < 4 then (
									let x = fst (pos0 cube [|- 1; - 1; 3|]) in
										if x = [|1; 1; 3|] then
											exe [pi; bi; m'; bi'; pi']
										else if x = [|1; - 1; 3|] then
											exe [pi; bi; m; bi'; pi'];
										while !i < 4 do
											exe [h0];
											incr i
										done;
										false
								)
							else (
									exe [pi; bi; m'; bi'; pi'];
									true
								)
					in
						
						if (faire_le_tour () && faire_le_tour ()) then
							raise Arranger_les_centres
		
		in
			exe [ah; d0; d0; ah; d0'; ah; a0; ah; a0; ah; a0; ah; a0; d0']
;;

(*-- fin de arranger les centres en vue d'une résolution complète du cube --*)


(*-- apparier les angles --*)

let est_a_apparier cube angle =
	let pos0, pos1 =
		let pos cube mv1 x =
			let p = cube.context1.matrice in
				let m = transpose p /./ (fun_of_mv1 mv1) (x /:/ transpose p) /./ p
				in
					x /:/ m, m
		in
			(fun cube x -> pos cube cube.mouvement1.mv1 x),
			(fun cube x -> pos cube (inverse cube.mouvement1.mv1) x)
	in
		let jumeau a =
			let f x = if x = 1 then - 1 else if x = - 1 then 1 else x in map_vect f a
		in
			fst (pos0 cube (jumeau (fst (pos1 cube angle)))) <> jumeau angle
;;

(* déplacement de 'angle' vers [|3;-3;1|] *)
let deplace_angle cube angle =
	let (OPS (a0, d0, h0), OPS (a0', d0', h0')) = cube.op_globales1
	in
		match vect angle with
			| 3, - 1, 3 -> exe [h0; d0']
			
			| 3, 1, 3 -> exe [a0'];
			| - 1, - 3, 3 -> exe [d0'];
			| 1, - 3, 3 -> exe [h0'; a0'];
			| - 1, 3, 3 -> exe [h0; a0'];
			| 1, 3, 3 -> exe [d0'; a0; a0];
			| - 3, - 1, 3 -> exe [h0; h0; a0'];
			| - 3, 1, 3 -> exe [h0'; d0'];
			
			| 3, - 1, - 3 -> exe [a0];
			| 3, 1, - 3 -> exe [a0; a0; h0; d0'];
			| - 1, - 3, - 3 -> exe [h0'; a0];
			| 1, - 3, - 3 -> exe [d0];
			| - 1, 3, - 3 -> exe [d0; a0; a0];
			| 1, 3, - 3 -> exe [h0; a0];
			| - 3, - 1, - 3 -> exe [h0'; d0];
			| - 3, 1, - 3 -> exe [a0; d0; d0];
			
			| - 3, - 3, - 1 -> exe [d0; d0];
			| - 3, - 3, 1 -> exe [d0'; h0'; a0'];
			| - 3, 3, - 1 -> exe [d0; h0; a0];
			| - 3, 3, 1 -> exe [d0; d0; h0; a0];
			| 3, 3, - 1 -> exe [a0; a0];
			
			| _ -> ()
;;

(* recherche du jumeau de l'angle situé en [|3;-3;1|]  et appariement *)
let apparier_angle_ag cube =
	let pos0, pos1 =
		let pos cube mv1 x =
			let pp = cube.context1.matrice in
				let m = transpose pp /./ (fun_of_mv1 mv1) (x /:/ transpose pp) /./ pp
				in
					x /:/ m, m
		in
			(fun cube x -> pos cube cube.mouvement1.mv1 x),
			(fun cube x -> pos cube (inverse cube.mouvement1.mv1) x)
	in
		let jumeau a =
			let f x = if x = 1 then - 1 else if x = - 1 then 1 else x in map_vect f a
		in
			let y, _ = pos0 cube (jumeau (fst (pos1 cube [|3; - 3; 1|])))
			and (OPS (a0, _, _), OPS (_, _, _)) = cube.op_globales1
			and (OPS (_, _, _), OPS (_, _, _), OPS (_, _, bi), OPS (_, _, bi')) = cube.op_externes1i
			and (OPS (a, d, h), OPS (a', d', h'), OPS (p, g, b), OPS (p', g', b')) = cube.op_externes1
			in
				(match vect y with
						| 3, - 1, 3 -> exe [g'; a; g]
						| 3, 1, 3 -> exe [h'; d']
						| - 1, - 3, 3 -> exe [h'; g'; a; g]
						| 1, - 3, 3 -> exe [h'; h'; d']
						| - 1, 3, 3 -> exe [d']
						| 1, 3, 3 -> exe [a'; h; a]
						| - 3, - 1, 3 -> exe [h; d']
						| - 3, 1, 3 -> exe [p'; d'; d']
						
						| 3, - 1, - 3 -> exe [b; d]
						| 3, 1, - 3 -> exe [g'; a'; g]
						| - 1, - 3, - 3 -> exe [g'; a; a; g]
						| 1, - 3, - 3 -> exe [g'; a; g; h'; d']
						| - 1, 3, - 3 -> exe [b'; g'; a'; g]
						| 1, 3, - 3 -> exe [d]
						| - 3, - 1, - 3 -> exe [b; b; g'; a'; g]
						| - 3, 1, - 3 -> exe [b'; d]
						
						| - 3, - 3, - 1 -> exe [p'; h; d']
						| - 3, - 3, 1 -> exe [p; p; d; d]
						| - 3, 3, - 1 -> exe [d; d]
						| - 3, 3, 1 -> exe [p; h; d']
						| 3, 3, - 1 -> exe [d; a'; h; a]
						
						| _ -> ()
				);
				if y <> [|3; - 3; - 1|] then
					exe [bi; d; a'; h; d'; a; bi'];
				exe [a0; a0]
;;

(* tant que nécessaire, déplacer un angle non apparié vers [|3;-3;1|], chercher son jumeau et apparier *)
let apparier_les_angles cube =
	let p = cube.context1.matrice
	and l = select (fun x -> est_angle x && (x.(0) = 1 || x.(1) = 1 || x.(2) = 1)) (map fst e)
	in
		(try
				while true do
					if est_a_apparier cube [|3; - 3; 1|] then
						apparier_angle_ag cube
					else
						let t = hd (select (est_a_apparier cube) l) in
							deplace_angle cube t;
							apparier_angle_ag cube;
				done;
			with Failure "hd" -> ()
		);
		cube.context1.matrice <- p;
		cube.dessine1 ()
;;

(*-- fin de apparier les angles --*)

(*- fin de étapes pour la résolution par niveaux du Rubik's cube 4x4 -*)


(*- début de résolution du cube 4x4 -*)

(* Résolution du cube 4x4 ne prenant en compte que les couleurs des faces *)
let resoudre_le_cube_4x4 cube =
	print_string "\nDÉBUT DE APPARIER LES ANGLES\n"; print_newline ();
	apparier_les_angles cube; (* but: réapparier les demi-angles *)
	print_string "\n\nFIN DE APPARIER LES ANGLES\n"; print_newline ();
	print_string "\nDÉBUT DE REGROUPER LES CENTRES\n"; print_newline ();
	regrouper_les_centres cube; (* regroupement par couleurs des centres sur une même face *)
	print_string "\n\nFIN DE REGROUPER LES CENTRES\n"; print_newline ();
	print_string "\n\nRÉSOLUTION DU CUBE 3 x 3 x 3 SOUS-JACENT"; print_newline ();
	resoudre_le_cube3x3 cube true false (* but: résolution du cube 3x3 avec tests sans rotation des centres *)
;;

(* résolution du cube 4x4 avec retour à la configuration usine *)
(* en principe 'regrouper_les_centres' et 'apparier_les_angles' commutent *)
let resoudre_completement_le_cube_4x4 cube =
	print_string "\nDÉBUT DE APPARIER LES ANGLES\n"; print_newline ();
	apparier_les_angles cube; (*but: réapparier les demi-angles *)
	print_string "\n\nFIN DE APPARIER LES ANGLES\n"; print_newline ();
	print_string "\nDÉBUT DE VERIFIER_ANGLES\n"; print_newline ();
	verifier_angles cube; (* rotation totale des angles appariés nulle *)
	print_string "\n\nFIN DE VERIFIER_ANGLES\n"; print_newline ();
	print_string "\nDÉBUT DE VERIFIER LES PARITÉS\n"; print_newline ();
	verifier_parites cube; (* même parité pour les permutations des angles et des coins *)
	print_string "\n\nFIN DE VERIFIER LES PARITÉS\n"; print_newline ();
	print_string "\nDÉBUT DE REGROUPER LES CENTRES\n"; print_newline ();
	regrouper_les_centres cube; (* regroupement par couleurs des centres sur une même face *)
	print_string "\n\nFIN DE REGROUPER LES CENTRES\n"; print_newline ();
	print_string "\nDÉBUT DE ARRANGER LES CENTRES\n"; print_newline ();
	arranger_les_centres cube; (* remettre chaque centre à sa place *)
	print_string "\n\nFIN DE ARRANGER LES CENTRES\n"; print_newline ();
	(* à ce stade les permutations d'angles et de coins du cube 3x3 devraient être paires *)
	print_string "\n\nRÉSOLUTION DU CUBE 3 x 3 x 3 SOUS-JACENT"; print_newline ();
	resoudre_le_cube3x3 cube false true (* résolution du cube 3x3 sans tests, avec rotation des centres *)
;;

let resoudre_completement_en_silence_le_cube_4x4 cube =
	apparier_les_angles cube; (* but: réapparier les demi-angles *)
	verifier_angles cube; (* rotation totale des angles appariés nulle *)
	verifier_parites cube; (* même parité pour les permutations des angles et des coins *)
	regrouper_les_centres cube; (* regroupement par couleurs des centres sur une même face *)
	arranger_les_centres cube; (* remettre chaque centre à sa place *)
	(* à ce stade les permutations d'angles et de coins du cube 3x3 devraient être paires *)
	resoudre_le_cube3x3 cube false true (* résolution du cube 3x3 sans tests, avec rotation des centres *)
;;

(*- fin de résolution du cube 4x4 -*)


(* test d'appartenance d'un mouvement au sous-groupe R *)
(* fondé sur la résolution par niveaux *)
let est_rubik m =
	let cube = nouveau_cube_muet {mv1 = m} in
		apparier_les_angles cube;
		verifier_angles cube;
		verifier_parites cube;
		regrouper_les_centres cube;
		arranger_les_centres cube;
		let _ = resoudre_le_cube3x3 cube false true
		in
			cube.mouvement1.mv1 = e
;;


(* EXEMPLES *)

let mv1 = mv1_rubik_r ();;
est_dans_R mv1;;
est_rubik mv1;;

(*- superflip -*)

let superflip =
	let cb = nouveau_cube_muet {mv1 = e} in
		let (OPS (a, d, h), OPS (a', d', h'), OPS (p, g, b), OPS (p', g', b')) = cb.op_externes1 in
			exe [d'; h; h; p; g'; a; h'; p; b; a; h; b'; g; b; b; a'; d; p'; b; a'; h'; p'; h; b'];
			map (fun (x, y) -> if est_angle x || est_coin x then x, y else x, id) cb.mouvement1.mv1
;;

let superflip1 =
	let demi_tour v = (* s'applique aux angles 'v' uniquement *)
		let w = map_vect (fun x -> x / 3) v in
			w /::/ w /+/ idm
	in
		mv1_of_fun (fun x -> if est_angle x then demi_tour x else id)
;;

let superflip2 = nouveau_mv1 (fun x -> x) (fun x -> map_vect (fun t -> if t * t = 1 then - t else t) x)
	(fun x -> x) (fun x -> 0);;

superflip1 = superflip;;
superflip2 = superflip1;;

(*- fin de superflip -*)


(*- superflip-four-spot  -*)

let superflip_four_spot = (* Thomas Rokicki 2014 *)
	let cb = nouveau_cube_muet {mv1 = e} in
		let (OPS (a, d, h), OPS (a', d', h'), OPS (p, g, b), OPS (p', g', b')) = cb.op_externes1 in
			exe [h; h; a; h; h; d'; g; a; a; h; a'; p'; d; g; h; h; d; h; b'; d; g'; b; d'; g'; b; b];
			map (fun (x, y) -> if est_angle x || est_coin x then x, y else x, id) cb.mouvement1.mv1
;;

let superflip_four_spot1 =
	let demi_tour v = (* s'applique aux angles 'v' uniquement *)
		let w = map_vect (fun x -> x / 3) v in
			(w /::/ w) /+/ idm
	and dt = diag (- 1) (- 1) 1
	in
		mv1_of_fun (
			fun x -> if est_angle x then demi_tour x /./ dt
							else if est_coin x then dt
							else id
		)
;;

superflip_four_spot1 = superflip_four_spot;;

(*- fin de superflip-four-spot -*)


(* boucle graphique *)
let boucle cube_x =
	boucle1 cube_x
	(
		fun () ->
						cube_x.liste_ops1 := [];
						regrouper_les_centres cube_x;
						dessine_cube cube_x.context1 cube_x.mouvement1.mv1;
						let l = list_length !(cube_x.liste_ops1) in
							printf__printf "\nCONSTRUIRE LES CENTRES: %d quarts de tour\n" l;
							print_newline ()
	)
	(
		fun () ->
						cube_x.liste_ops1 := [];
						apparier_les_angles cube_x;
						dessine_cube cube_x.context1 cube_x.mouvement1.mv1;
						let l = list_length !(cube_x.liste_ops1) in
							printf__printf "\nAPPARIER LES ANGLES: %d quarts de tour\n" l;
							print_newline ()
	)
	(
		fun () ->
						cube_x.liste_ops1 := [];
						let liste_ops =
							resoudre_le_cube_4x4 cube_x
						in
							let l = list_length liste_ops in
								dessine_cube cube_x.context1 cube_x.mouvement1.mv1;
								printf__printf "\nRÉSOLUTION SIMPLE DU CUBE 4 x 4 x 4 en %d quarts de tour\n" l;
								print_newline ();
	)
	(
		fun () ->
						cube_x.liste_ops1 := [];
						let liste_ops =
							resoudre_completement_le_cube_4x4 cube_x
						in
							let l = list_length liste_ops in
								dessine_cube cube_x.context1 cube_x.mouvement1.mv1;
								printf__printf "\nRÉSOLUTION COMPLÈTE DU CUBE 4 x 4 x 4 en %d quarts de tour\n" l;
								print_newline ();
	)
	(
		fun () -> (*melanger cube_x;*) cube_x.mouvement1.mv1 <- mv1_rubik_r ();
						cube_x.liste_ops1 := []; cube_x.dessine1 ()
	)
;;

let cube = nouveau_cube_graphique {mv1 = superflip_four_spot};;
boucle cube;;

let cube = nouveau_cube_verbeux {mv1 = superflip_four_spot};;
resoudre_le_cube_4x4 cube;;


(*- DIVERS --------------------------------------------------------------------------------------------------------------------------------------*)

(*
Pour utiliser directement ce qui suit, interrompre la boucle ci-dessus et procéder par lignes entières.
   Sélectionner et envoyer ensemble les 12 lignes non vides suivantes...

graphics__open_graph " 630x800";;
let cube = nouveau_cube_graphique {mv1 = mv1};;
cube.dessine1();;
let (OPS (a0, d0, h0), OPS(a0', d0', h0')) = cube.op_globales1;;
let (OPS (ai, di, hi), OPS(ai', di', hi'), OPS(pi, gi, bi), OPS(pi', gi', bi')) = cube.op_externes1i;;
let (OPS (a, d, h), OPS(a', d', h'), OPS(p, g, b), OPS(p', g', b')) = cube.op_externes1;;
let (OPS (orangeI, vertI, blancI), OPS(orangeI', vertI', blancI'), OPS(rougeI, bleuI, jauneI), OPS(rougeI', bleuI', jauneI')) = cube.op_internes1i;;
let (OPS (orange, vert, blanc), OPS(orange', vert', blanc'), OPS(rouge, bleu, jaune), OPS(rouge', bleu', jaune')) = cube.op_internes1;;
let aai () = exe [a; ai] and ddi () = exe [d; di] and hhi () = exe [h; hi]
and aai' () = exe [a'; ai'] and ddi' () = exe [d'; di'] and hhi' () = exe [h'; hi']
and ppi () = exe [p; pi] and ggi () = exe [g; gi] and bbi () = exe [b; bi]
and ppi' () = exe [p'; pi'] and ggi' () = exe [g'; gi'] and bbi' () = exe [b'; bi'];;

... puis exécuter une par une certaines des commandes qui suivent : (sélectionner une ligne et l'envoyer)


a0();;
a0'();;

d0();;
d0'();;

h0();;
h0'();;


a();;
a'();;

p();;
p'();;

d();;
d'();;

g();;
g'();;

h();;
h'();;

b();;
b'();;


ai();;
ai'();;

pi();;
pi'();;

di();;
di'();;

gi();;
gi'();;

hi();;
hi'();;

bi();;
bi'();;


aai();;
aai'();;

ppi();;
ppi'();;

ddi();;
ddi'();;

ggi();;
ggi'();;

hhi();;
hhi'();;

bbi();;
bbi'();;

orange();;
orange'();;

blanc();;
blanc'();;

vert();;
vert'();;

rouge();;
rouge'();;

bleu();;
bleu'();;

jaune();;
jaune'();;

orangeI();;
orangeI'();;

blancI();;
blancI'();;

vertI();;
vertI'();;

rougeI();;
rougeI'();;

bleuI();;
bleuI'();;

jauneI();;
jauneI'();;


(* séquences propres au cube 4x4 *)
  (* séquence utilisée pour regrouper les centres par couleurs, puis pour arranger les centres dans chaque face *)
  (* [[ai,bi]h'] *)
let l = [ai; bi; ai'; bi'; h'; bi; ai; bi'; ai'; h] in exe l;;
  (* et son inverse [h'[ai,bi]] *)
let l' = [h'; ai; bi; ai'; bi'; h; bi; ai; bi'; ai'] in exe l';;

  (* autre séquence utilisée pour arranger les centres regroupés par couleurs dans chaque face *)
  (* m() et m'() sont les deux séquences précédemment utilisées pour les centres *)      
let l' = [h'; ai; bi; ai'; bi'; h; bi; ai; bi'; ai'] in let m'() = exe l' in exe [pi; bi; m'; bi'; pi'];;
  (* et son inverse *)
let l = [ai; bi; ai'; bi'; h'; bi; ai; bi'; ai'; h] in let m() = exe l in exe [pi; bi; m; bi'; pi'];;

  (* séquence utilisée pour le jumelage des angles sur la face antérieure côté gauche *)
  (* après avoir placé les jumeaux en vis à vis vers le haut : positions [|3;-3;1|] et [|3;3;1|] *)
exe [bi; d; a'; h; d'; a; bi'];;
  (* et son inverse *)
exe [bi; a'; d; h'; a; d'; bi'];;

*)

(*- FIN DE DIVERS -------------------------------------------------------------------------------------------------------------------------------*)
