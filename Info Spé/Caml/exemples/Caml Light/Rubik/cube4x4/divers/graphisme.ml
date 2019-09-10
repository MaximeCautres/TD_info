(* projection du cube sur l'écran *)
(* dans 'xx v' et 'yy v', 'v' sert à décaler les projections des faces cachées *)
(* 'x,y,z' sont les coordonnées ADH *)

let ux = 20;;
let uy = 20;;
let ox = 333;;
let oy = 400;;

let proj x y z =
	let c = sqrt 6. /. 2. in
		(c *. (y -. x) /. sqrt 2., c *. (-. (x +. y) +. 2. *. z) /. sqrt 6.)
;;

let xx v pt3 =
	let (x, y, z) = vect (map_vect float_of_int pt3) in
		let (x1, y1, z1) =
			if v /|/ [|1; 1; 1|] = 1 then (x, y, z)
			else match vect v with
					| (_, 0, 0) -> (x -. 8., y, z)
					| (0, _, 0) -> (x, y -. 8., z)
					| _ -> (x, y, z -. 8.)
		in
			int_of_float (float_of_int ox +. fst (proj x1 y1 z1) *. float_of_int ux)
;;

let yy v pt3 =
	let (x, y, z) = vect (map_vect float_of_int pt3) in
		let (x1, y1, z1) =
			if v /|/ [|1; 1; 1|] = 1 then (x, y, z)
			else match vect v with
					| (_, 0, 0) -> (x -. 8., y, z)
					| (0, _, 0) -> (x, y -. 8., z)
					| _ -> (x, y, z -. 8.)
		in
			int_of_float (float_of_int oy +. snd (proj x1 y1 z1) *. float_of_int uy)
;;

(* numérotation des centres *)
let numero c = let est_centre i = (i /|/ i) = 11
	in
		if est_centre c then string_of_int (assoc c
				[
					[|- 1; - 1; 3|], 1; [|- 1; 1; 3|], 2; [|1; 1; 3|], 3; [|1; - 1; 3|], 0;
					[|- 1; - 1; - 3|], 0; [|- 1; 1; - 3|], 3; [|1; 1; - 3|], 2; [|1; - 1; - 3|], 1;
					
					[|3; - 1; - 1|], 0; [|3; - 1; 1|], 1; [|3; 1; 1|], 2; [|3; 1; - 1|], 3;
					[|- 1; - 3; - 1|], 0; [|- 1; - 3; 1|], 1; [|1; - 3; 1|], 2; [|1; - 3; - 1|], 3;
					[|- 3; - 1; - 1|], 1; [|- 3; - 1; 1|], 0; [|- 3; 1; 1|], 3; [|- 3; 1; - 1|], 2;
					[|- 1; 3; - 1|], 3; [|- 1; 3; 1|], 2; [|1; 3; 1|], 1; [|1; 3; - 1|], 0
				])
		else ""
;;

(* Simulation graphique du Rubik's cube *)

(* la fonction 'drawPoly' est utilisée pour tracer le pourtour des projections *)
(* des faces des minicubes *)
let drawPoly poly =
	let (x, y) = poly.(0) in graphics__moveto x y;
		for i = 1 to vect_length poly - 1 do
			let (x, y) = poly.(i) in graphics__lineto x y
		done;
		let (x, y) = poly.(0) in graphics__lineto x y;
;;

(* la fonction 'draw' est utilisée pour dessiner la projection 'x' d'une face *)
(* de minicube en superposant le tracé du pourtour à la couleur de remplissage *)
(* 'draw' avec inscription des numéros des centres dans leur position actuelle *)
let draw x =
	let a, b, c = x in
		graphics__set_color b;
		graphics__fill_poly a;
		graphics__set_color graphics__black;
		drawPoly a;
		let
		((a1, b1), (a2, b2), (a3, b3), (a4, b4)) = (a.(0), a.(1), a.(2), a.(3))
		in
			let (c1, c2) = ((a1 + a3) / 2, (b1 + b3) / 2)
			in
				graphics__moveto c1 c2;
				graphics__draw_string c

;;

(* 'draw1' inscrit les numéros des emplacements usine des centres *)
let draw1 x =
	let a, b, c = x and abs x = if x > 0 then x else - x in
		graphics__set_color (graphics__rgb 100 100 100);
		let
		((a1, b1), (a2, b2), (a3, b3), (a4, b4)) = (a.(0), a.(1), a.(2), a.(3))
		in
			let (c1, c2) = ((a1 + a3 - abs (a2 - a3) + 4) / 2, (b1 + b3 - abs (b2 - b3) + 4) / 2)
			in
				graphics__moveto c1 c2;
				graphics__draw_string c

;;

(* la fonction 'draw_face' est utilisée pour dessiner la projection d'une face du *)
(* rubik's cube en appliquant 'draw' aux projections des faces des minicubes *)
(* qu'elle contient. Elle prend en paramètre le résultat de 'proj_face' ci-dessous *)

let rec draw_face l = match l with
		| t :: r -> draw t; draw_face r
		| [] -> ()
;;

let rec draw_face1 l = match l with
		| t :: r -> draw1 t; draw_face1 r
		| [] -> ()
;;

(* les trois fonctions suivantes utilisent le repère du cube *)

(* 'face v c' renvoie, si le minicube à l'emplacement d'indice 'c' a une face F *)
(* dans la face du Rubik's cube normale au vecteur sortant 'v', sous forme de vecteur *)
(* une liste circulaire des 4 sommets de F *)
let face v c =
	let e = v /|/ [|1; 1; 1|] in let w = [|e; e; e|] in
			let w1 = w /:/ rot v in
				let w2 = w1 /:/ rot v in
					let w3 = w2 /:/ rot v in
						let l = [w; w1; w2; w3] in
							let add m = for i = 0 to 2 do m.(i) <- m.(i) + c.(i) done
							in
								do_list add l; vect_of_list l
;;

(* pour la face F du rubick's cube repérée par le vecteur normal sortant 'v', *)
(* 'face_color' associe à chaque minicube appartenant à cette face F et *)
(* repéré par son indice la couleur qu'il présente sur F *)
(* ainsi que son numéro de 0 à 3 dans sa couleur si c'est un centre *)
(* et renvoie la liste d'associations *)
let face_colors v minicubes =
	let f x = (fst x) /:/ (snd x) in
		map (fun x -> (f x, (couleur_de_face (v /:/ transpose (snd x))), numero (fst x)))
		(select (fun x -> f x /|/ v = 3 && fst x /|/ fst x <> 9) minicubes)
;;

(* 'face_prepare' fournit, pour la face du rubik's cube normale au vecteur *)
(* sortant 'v', une liste de triplets: les triplets correspondent aux minicubes *)
(* dont une face F est incluse dans celle du Rubik's cube; la première *)
(* composante est sous forme de vecteur une liste circulaire des 4 sommets *)
(* de F, la seconde composante est la couleur de F et la troisième dans le cas *)
(* d'un centre identifie par son numéro ce dernier dans sa couleur *)
let face_prepare v minicubes =
	let l = face_colors v minicubes in
		map (fun (x, y, z) -> (face v x, y, z)) l
;;

(* on introduit dans les deux fonctions suivantes la matrice de passage 'p' *)
(* du repère ADH au repère OVB du cube : (x=x'p) où x est la ligne des coordonnées *)
(* d'espace et x' la ligne des coordonnées par rapport au cube *)

(* 'proj_face' renvoie, comme 'face_prepare', une liste de triplets *)
(* 'contour * couleur * numéro' correspondant cette fois aux PROJECTIONS *)
(* des faces de minicubes contenues dans la face du Rubik's cube *)
(* orthogonale au vecteur sortant 'v' *)
let proj_face p mv1 v =
	let f = face_prepare v mv1 and
	proj_vect l =
		map_vect (fun x -> xx (v /:/ p) (x /:/ p), yy (v /:/ p) (x /:/ p)) l
	in
		map (fun (x, y, z) -> proj_vect x, y, z) f
;;

(* 'dessine_cube context mv1' dessine le cube dans l'état 'mv1' *)
let dessine_cube context mv1 =
	let p = context.matrice and m = mv1 in
	(*faces opposées*)
		do_vect (fun x -> draw_face (proj_face p m x)) idm;
		(*faces frontales*)
		do_vect (fun x -> draw_face (proj_face p m x)) id;
		
		(* pour les numéros des emplacements par défaut des centres *)
		let e = map (fun x -> x, id) indices in
		(*faces opposées *)
			do_vect (fun x -> draw_face1 (proj_face p e x)) idm;
			(*faces frontales*)
			do_vect (fun x -> draw_face1 (proj_face p e x)) id;

;;
