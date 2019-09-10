include "exemples/Caml Light/Rubik/cube2x2/divers/types.ml";;
include "exemples/Caml Light/Rubik/divers/divers.ml";;
include "exemples/Caml Light/Rubik/cube2x2/divers/section_marques.ml";;
include "exemples/Caml Light/Rubik/divers/couleurs.ml";;
include "exemples/Caml Light/Rubik/cube2x2/divers/graphisme.ml";;
include "exemples/Caml Light/Rubik/cube2x2/divers/boutons.ml";;


(* liste des coins  ici il n'y a que des coins! *)
let coins = indices;;

(* numérotation des coins *)
let v_coins = vect_of_list coins;;
let num_of_coin c =
	let i = ref 0 in
		while c <> v_coins.(!i) do
			incr i
		done;
		!i
;;
let coin_of_num i = v_coins.(i);;

(* identificateurs pour les numéros de coins *)
let adh = num_of_coin [|1; 1; 1|];;
let adb = num_of_coin [|1; 1; - 1|];;
let agb = num_of_coin [|1; - 1; - 1|];;
let agh = num_of_coin [|1; - 1; 1|];;
let pdh = num_of_coin [|- 1; 1; 1|];;
let pdb = num_of_coin [|- 1; 1; - 1|];;
let pgb = num_of_coin [|- 1; - 1; - 1|];;
let pgh = num_of_coin [|- 1; - 1; 1|];;


(*- groupe M des mouvements 'mv2' -*)

(* loi de groupe '$' pour les mouvements 'mv2' (produit semi-direct) *)
let prefix $ m m' =
	let nc = vect_length m.rot_coins and npc = vect_length m.perm_coins
	in let rc = make_vect nc 0 and pc = make_vect npc 0
		in
			for i = 0 to nc - 1 do
				rc.(i) <- (m.rot_coins.(i) + m'.rot_coins.(m.perm_coins.(i))) mod 3
			done;
			for i = 0 to npc - 1 do
				pc.(i) <- m'.perm_coins.(m.perm_coins.(i))
			done;
			{rot_coins = rc; perm_coins = pc}
;;

(* élément neutre *)
let e2 = {
		rot_coins = [|0; 0; 0; 0; 0; 0; 0; 0|];
		perm_coins = [|0; 1; 2; 3; 4; 5; 6; 7|]}
;;

(* permutation inverse de la permutation d'entiers i -> s.(i) *)
let perm_inverse s = let n = vect_length s in
		let t = make_vect n 0 in
			for i = 0 to n - 1 do
				t.(s.(i)) <- i
			done;
			t
;;

(* inverse d'un élément de M *)
let inv mv2 =
	let rc = mv2.rot_coins
	and pci = perm_inverse mv2.perm_coins in
		let tc = make_vect 8 0 in
			for i = 0 to 7 do
				tc.(i) <- (3 - rc.(pci.(i))) mod 3
			done;
			{rot_coins = tc; perm_coins = pci}
;;

(* test d'appartenance d'un mouvement au sous-groupe de Rubik R de M *)
let est_dans_R mvs =
	let sum v = let s = ref 0 and n = vect_length v in
			for i = 0 to n - 1 do
				s := !s + v.(i)
			done;
			!s
	in
		(sum mvs.rot_coins) mod 3 = 0;;

(*- fin de groupe M des mouvements 'mv2' -*)


(*- mouvement aléatoire -*)

random__init (unix__time ());;

(* mouvement aléatoire général *)
let mv2_r () =
	let rc () =
		let v = make_vect 8 0 in
			for i = 0 to 7 do v.(i) <- random__int 3 done;
			v
	and random_vect n = vect_of_list (random_list (liste n))
	in
		let pc () = random_vect 8
		in
			{rot_coins = rc (); perm_coins = pc ()}
;;

(* mouvement de Rubik aléatoire *)
let mv2_rubik_r () =
	let mv2 = mv2_r ()
	and sum v = let s = ref 0 and n = vect_length v in
			for i = 0 to n - 1 do
				s := !s + v.(i)
			done;
			!s
	in
		(let s = (sum mv2.rot_coins) mod 3 in
				if s <> 0 then mv2.rot_coins.(0) <- (mv2.rot_coins.(0) + 3 - s) mod 3);
		mv2
;;

(*- fin de mouvement aléatoire -*)


(*- conversions entre mouvements 'mv1' et mouvements 'mv2'... -*)

(* rotations des coins: liste d'exposants *)
(* pour mv1_of_mv2 *)
let l_rtc m =
	let rtc_aux k = let f = fun_of_mv1 k in
			let indexc i = if f i = st i then 1
				else if f i = transpose (st i) then 2 else 0 in
				map indexc coins
	in rtc_aux (ker m)
;;

(* ...pour définir les opérations de rubik comme mouvements 'mv2'... *)
let mv2_of_mv1 mv1 =
	let rc = vect_of_list (l_rtc mv1)
	and pc = vect_of_list (map num_of_coin (map (sur mv1) (coins)))
	in
		{rot_coins = rc; perm_coins = pc}
;;

(* ...et pour utiliser 'dessine_cube' *)
let mv1_of_mv2 mv2 =
	let ec = fun i -> mv2.rot_coins.(num_of_coin i)
	and pc = fun i -> coin_of_num (mv2.perm_coins.(num_of_coin i))
	in
		nouveau_mv1 pc ec
;;

(*- fin de conversions entre mouvements 'mv1' et mouvements 'mv2' -*)


(*- introduction des mouvements élémentaires de Rubik de type 'mv2' -*)

let rub_a = mv2_of_mv1 (rub [|1; 0; 0|]);;
let rub_d = mv2_of_mv1 (rub [|0; 1; 0|]);;
let rub_h = mv2_of_mv1 (rub [|0; 0; 1|]);;
let rub_a' = mv2_of_mv1 (rub' [|1; 0; 0|]);;
let rub_d' = mv2_of_mv1 (rub' [|0; 1; 0|]);;
let rub_h' = mv2_of_mv1 (rub' [|0; 0; 1|]);;

let rub_p = mv2_of_mv1 (rub [|- 1; 0; 0|]);;
let rub_g = mv2_of_mv1 (rub [|0; - 1; 0|]);;
let rub_b = mv2_of_mv1 (rub [|0; 0; - 1|]);;
let rub_p' = mv2_of_mv1 (rub' [|- 1; 0; 0|]);;
let rub_g' = mv2_of_mv1 (rub' [|0; - 1; 0|]);;
let rub_b' = mv2_of_mv1 (rub' [|0; 0; - 1|]);;

let rub1 x = match vect x with
		| 1, 0, 0 -> rub_a
		| 0, 1, 0 -> rub_d
		| 0, 0, 1 -> rub_h
		| - 1, 0, 0 -> rub_p
		| 0, - 1, 0 -> rub_g
		| 0, 0, - 1 -> rub_b
		| _ -> failwith "rub1"
;;

let rub1' x = match vect x with
		| 1, 0, 0 -> rub_a'
		| 0, 1, 0 -> rub_d'
		| 0, 0, 1 -> rub_h'
		| - 1, 0, 0 -> rub_p'
		| 0, - 1, 0 -> rub_g'
		| 0, 0, - 1 -> rub_b'
		| _ -> failwith "rub1'"
;;

(*- fin de introduction des mouvements élémentaires de Rubik de type 'mv2' -*)


(*- résolution par niveaux du Rubik's cube 2x2 -*)

(* mouvements globaux du cube et conjugués *)
let cste c = mv2_of_mv1 (map (fun x -> x, c) indices);;
let conj p m = p $ m $ inv p;;
let conjc c m = conj (cste c) m;;

(* Initialisation du Rubik's cube : mise en place des mouvements élémentaires de Rubik *)
let nouveau_cube2 mouvement context dessine liste_mouvements =
	
	let listeops = ref []
	and dessine () = dessine context (mv1_of_mv2 mouvement.mv2)
	in
		let op_externes liste_ops =
			let fct x () =
				(*mouvement.mv2 <- mouvement.mv2 $ conjc context.matrice (rub1 x);*)
				let t = x /:/ transpose context.matrice in
					mouvement.mv2 <- mouvement.mv2 $ rub1 t;
					if liste_mouvements then (
							let t = x /:/ transpose context.matrice in
								print_string (nom_de_face t ^ " ");
								liste_ops := !liste_ops @ [nom_de_face t];
						);
					dessine ()
			and fct' x () =
				(*mouvement.mv2 <- mouvement.mv2 $ conjc context.matrice (rub1' x);*)
				let t = x /:/ transpose context.matrice in
					mouvement.mv2 <- mouvement.mv2 $ rub1' t;
					if liste_mouvements then (
							let t = x /:/ transpose context.matrice in
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
		
		and op_internes () =
			let fct x () =
				mouvement.mv2 <- mouvement.mv2 $ rub1 x;
				print_string (nom_de_face x ^ " ");
				dessine ()
			and fct' x () =
				mouvement.mv2 <- mouvement.mv2 $ rub1' x;
				if liste_mouvements then print_string (nom_de_face x ^ "' ");
				dessine ()
			in
				let (o, v, blanc) = vect (map_vect fct id)
				and (o', v', blanc') = vect (map_vect fct' id)
				and (r, b, j) = vect (map_vect fct idm)
				and (r', b', j') = vect (map_vect fct' idm)
				in (OPS (o, v, blanc), OPS (o', v', blanc'), OPS (r, b, j), OPS (r', b', j'))
		
		and op_globales () =
			let rotation pp () = context.matrice <- context.matrice /./ pp;
				dessine () in
				let (a, d, h) = vect (map_vect rotation (map_vect rot id))
				and (a', d', h') = vect (map_vect rotation (map_vect rot' id))
				in
					(OPS (a, d, h), OPS (a', d', h'))
		
		in
			let op_ext = op_externes listeops and op_int = op_internes () in
				let op_from_strings liste_ops =
					let (OPS (orange, vert, blanc), OPS (orange', vert', blanc'), OPS (rouge,
					bleu, jaune), OPS (rouge', bleu', jaune')) = op_int
					in
						let aux s = assoc s
							[("orange", orange); ("vert", vert); ("blanc", blanc);
								("orange'", orange'); ("vert'", vert'); ("blanc'", blanc');
								("rouge", rouge); ("bleu", bleu); ("jaune", jaune);
								("rouge'", rouge'); ("bleu'", bleu'); ("jaune'", jaune')]
						in
							let rec op_from_strings_aux liste_ops =
								match liste_ops with
									t :: r -> aux t :: op_from_strings_aux r
									| [] -> []
							in op_from_strings_aux liste_ops
				in
					{mouvement2 = mouvement; context2 = context; dessine2 = dessine;
						op_globales2 = op_globales (); op_externes2 = op_ext;
						op_internes2 = op_int; liste_ops2 = listeops;
						op_from_strings2 = op_from_strings;
						boutons2 = make_vect 1 {titre = ""; orx = 0; ory = 0; largeur = 0;
							hauteur = 0; couleur = 0; action = fun () -> ()}
					}
;;

(* Résolution du cube 2x2 *)
exception Orienter_les_coins;;
exception Descendre_coin;;
exception Remonter_coin;;
exception Placer_les_coins;;

let resoudre_le_cube cube =
	let
	(OPS (a, d, h), OPS (a', d', h'), OPS (p, g, b), OPS (p', g', b')) = cube.op_externes2
	and
	(OPS (_, _, h0), OPS (_, _, h0')) = cube.op_globales2
	
	and coin_reel c = num_of_coin (coin_of_num c /:/ transpose (cube.context2.matrice))
	and deplacement_coin c =
		let v = coin_of_num (cube.mouvement2.mv2.perm_coins.(c)) in
			num_of_coin (v /:/ cube.context2.matrice)
	
	in
		
		let niveau_superieur () =
			
			(* niveau supérieur *)
			let placer_et_orienter_les_coins () =
				let descendre_coin () =
					let c1 = deplacement_coin (coin_reel adh) in
						if c1 = pdh then exe [p'; b'; p] else
						if c1 = pgh then exe [p; b; b; p'] else
						if c1 = agh then exe [g; b; g'] else
						if c1 = adh then exe [a; b; a'; b'] else
						if c1 = pdb then exe [b'] else
						if c1 = pgb then exe [b; b] else
						if c1 = agb then exe [b] else
						if c1 = adb then ()
						else raise Descendre_coin
				
				and remonter_coin () =
					let c1 = coin_reel adh in
						let c2 = deplacement_coin c1 in
							if c2 = adh && cube.mouvement2.mv2.rot_coins.(c2) = 0 then ()
							else (
									let etat1 = cube.mouvement2.mv2 $ conjc cube.context2.matrice rub_a'
									and etat2 = cube.mouvement2.mv2 $ conjc cube.context2.matrice rub_d in
										if etat1.rot_coins.(c1) = 0 then exe [a'; d; a; d'] else
										if etat2.rot_coins.(c1) = 0 then exe [d; a'; d'; a] else
											exe [a; b'; a'; b; b; d; a'; d'; a]
								)
				in
					for i = 0 to 3 do
						descendre_coin ();
						remonter_coin ();
						exe [h0]
					done;
			
			in
				placer_et_orienter_les_coins ();
		
		and niveau_inferieur () =
			
			(* niveau inférieur *)
			
			let placer_les_coins () =
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
							while !i < 4 && deplacement_coin (coin_reel adb) <> adb do
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
							let c1 = deplacement_coin (coin_reel pgb) in
								if c1 = agb then permuter () else
								if c1 = pdb then permuter' () else
								if c1 = pgb then ()
								else raise Placer_les_coins
			
			and orienter_les_coins () =
				let faire_tourner () =
					(* fait tourner les coins frontaux inférieurs sur eux-mêmes: 
          le coin gauche dans le sens direct, le coin droit en sens inverse *)
					exe [p'; b'; p; b'; p'; b; b; p];
					exe [a; b; a'; b; a; b; b; a']
				and faire_tourner' () =
					(* fait tourner les coins frontaux inférieurs sur eux-mêmes:
          le coin droit dans le sens direct, le coin gauche en sens inverse *)
					exe [a; b; b; a'; b'; a; b'; a'];
					exe [p'; b'; b'; p; b; p'; b; p]
				and rot_coin coin = (* coin supposé en place *)
					let m = cube.mouvement2.mv2 in
						m.rot_coins.(m.perm_coins.(coin_reel coin))
				
				in
					let orienter_frontal_inferieur_droit () =
						let n = rot_coin adb in
							if n = 0 then () else
							if n = 1 then faire_tourner' () else
							if n = 2 then faire_tourner () else
								raise Orienter_les_coins
					in
						for i = 0 to 2 do
							orienter_frontal_inferieur_droit ();
							exe [h0']
						done;
			
			in
			(* faire un quart de tour si la permutation des coins est impaire *)
			(* car 'placer les coins' suppose cette permutation paire *)
				let signature p =
					sign (liste (vect_length p)) (fun i -> p.(i))
				in
					if (signature cube.mouvement2.mv2.perm_coins = - 1) then exe [b]
					else ();
					placer_les_coins ();
					orienter_les_coins ()
		in
			cube.liste_ops2 := [];
			try
				let ctx = cube.context2.matrice in
					niveau_superieur ();
					niveau_inferieur ();
					cube.context2.matrice <- ctx;
					cube.dessine2 ();
					!(cube.liste_ops2)
			with
				| Orienter_les_coins ->
							print_string "erreur dans orienter_les_coins\n"; !(cube.liste_ops2)
				| Placer_les_coins ->
							print_string "erreur dans placer_les_coins\n"; !(cube.liste_ops2)
;;

(*- fin de résolution par niveaux du Rubik's cube 2x2 -*)

(* cube muet invisible dans l'état 'mv2'  orienté de façon standard *)
(* utilisé par la fonction 'est_rubik' *)
let nouveau_cube2_muet mv2 =
	nouveau_cube2 mv2 {matrice = id} (fun _ _ -> ()) false
;;

(* cube invisible dans l'état 'mv2' orienté de façon standard *)
(* écrivant les mouvements de ses faces - quarts de tours - *)
(* et les renvoyant sous forme de liste *)
let nouveau_cube2_verbeux mv2 =
	nouveau_cube2 mv2 {matrice = id} (fun _ _ -> ()) true
;;

(* cube avec affichage graphique dans l'état 'mv2' orienté de façon standard *)
(* suppose l'ouverture préalable de la fenêtre graphique pour fonctionner *)
let nouveau_cube2_graphique mv2 =
	nouveau_cube2 mv2 {matrice = id} dessine_cube true
;;

(* test d'appartenance d'un mouvement au sous-groupe R *)
(* fondé sur la résolution par niveaux *)
let est_rubik mv2 =
	let mouvement = {mv2 = mv2} in
		let _ = resoudre_le_cube (nouveau_cube2_muet mouvement)
		in
			mouvement.mv2 = e2
;;

(* cube avec mouvement résultant d'une séquence aléatoires de *)
(* mouvements de Rubik élémentaires *)
let melanger cube =
	let (OPS (a, d, h), OPS (a', d', h'), OPS (p, g, b), OPS (p', g', b')) = cube.op_externes2
	and s = vect_of_list (random_list (liste 12))
	and v = make_vect 12 (fun () -> ()) in
		v.(s.(0)) <- a; v.(1) <- d; v.(2) <- h; v.(s.(3)) <- a'; v.(s.(4)) <- d'; v.(s.(5)) <- h';
		v.(s.(6)) <- p; v.(s.(7)) <- g; v.(s.(8)) <- b; v.(s.(9)) <- p'; v.(s.(10)) <- g'; v.(s.(11)) <- b';
		let t = make_vect 30 (fun () -> ()) in
			for i = 0 to 29 do
				t.(i) <- v.(random__int 12);
			done;
			exe (list_of_vect t)
;;


(* EXEMPLES *)

let mv2 = mv2_rubik_r ();;
est_dans_R mv2;;
est_rubik mv2;;

let cube = nouveau_cube2_graphique {mv2 = mv2};;
boucle2 cube
(
	fun () -> let n = list_length (resoudre_le_cube cube) in
						(printf__printf "\nnombre de quarts de tour: %d\n" n;
							print_newline ()
						)
)
(
	fun () -> cube.mouvement2.mv2 <- mv2_rubik_r (); cube.dessine2 ()
					(*  fun () -> melanger cube *)
)
;;

let cube = nouveau_cube2_verbeux {mv2 = mv2};;
resoudre_le_cube cube;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(* Pour utiliser directement ce qui suit, interrompre la boucle ci-dessus et procéder par lignes entières.
   Sélectionner et envoyer ensemble les 6 lignes suivantes (let cube = ...) ...

let cube = nouveau_cube2_graphique {mv2 = mv2};;
graphics__open_graph " 612x612";;
cube.dessine2();;
let (OPS (a0, d0, h0), OPS(a0', d0', h0')) = cube.op_globales2;;
let (OPS (a, d, h), OPS(a', d', h'), OPS(p, g, b), OPS(p', g', b')) = cube.op_externes2;;
let (OPS (orange, vert, blanc), OPS(orange', vert', blanc'), OPS(rouge, bleu, jaune), OPS(rouge', bleu', jaune')) = cube.op_internes2;;

... puis exécuter une par une certaines des commandes qui suivent : (sélectionner une ligne puis l'envoyer)


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


orange();;
orange'();;

rouge();;
rouge'();;

vert();;
vert'();;

bleu();;
bleu'();;

blanc();;
blanc'();;

jaune();;
jaune'();;

*)


(*----------------- Quelques formules utilisées pour résoudre le cube 2x2 --------------------*)

(*

  (* NIVEAU SUPÉRIEUR face haute, blanche en principe *)

  (* placement du coin supérieur frontal droit à partir d'en dessous: face blanche *)      
  (* vers le bas : [a,b']b[d a'] *)
exe [a;b';a';b;b;d;a';d';a];;
  (* mouvement inverse *)
exe (rev[a';b;a;b;b;d';a;d;a']);;

  (* placement du coin supérieur frontal droit à partir de : face blanche frontale *)
  (* avec autre face correcte *)
exe [d;a';d';a];;
  (* mouvement inverse *)
exe [a';d;a;d'];;

  (* placement du coin supérieur frontal droit à partir de : face blanche à droite *)
  (* avec autre face correcte *)
exe [a';d;a;d'];;


  (* NIVEAU INFÉRIEUR (le précédent déjà fait) *)

  (* PERMUTER LES COINS *)

  (* laisse fixe le coin frontal droit et permute circulairement les autres : *)
  (* [p' b.a] *)
exe [p';b;a;b';p;b;a';b'];;
  (* mouvement inverse *)
exe (rev [p;b';a';b;p';b';a;b]);;

  (* ORIENTER LES COINS *)

  (* fait tourner les coins frontaux sur eux-mêmes: le coin gauche *)
  (* dans le sens des aiguilles d'une montre le coin droit en sens inverse *)
  (* (ab).[b,a'] (p'b').[b',p] *)

exe [a;b;b;a';b';a;b';a'];;
exe [p';b';b';p;b;p';b;p];;
  (* mouvement inverse *)
exe [p';b';p;b';p';b;b;p];;
exe [a;b;a';b;a;b;b;a'];;

*)