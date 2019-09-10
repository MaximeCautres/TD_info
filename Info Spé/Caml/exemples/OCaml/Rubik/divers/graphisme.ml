(* projection sur écran *)

let ux = 20;;
let uy = 20;;
let ox = 306;;
let oy = 320;;

let proj x y z =
	let c = sqrt 6. /. 2. in
		(c *. (y -. x) /. sqrt 2., c *. (-. (x +. y) +. 2. *. z) /. sqrt 6.)
;;

let xx v pt3 =
	let (x, y, z) = vect (map_vect float_of_int pt3) in
		let (x1, y1, z1) =
			if v /|/ [|1; 1; 1|] = 1 then (x, y, z)
			else match vect v with
					| (_, 0, 0) -> (x -. 7., y, z)
					| (0, _, 0) -> (x, y -. 7., z)
					| _ -> (x, y, z -. 7.)
		in
			int_of_float (float_of_int ox +. fst (proj x1 y1 z1) *. float_of_int ux)
;;

let yy v pt3 =
	let (x, y, z) = vect (map_vect float_of_int pt3) in
		let (x1, y1, z1) =
			if v /|/ [|1; 1; 1|] = 1 then (x, y, z)
			else match vect v with
					| (_, 0, 0) -> (x -. 7., y, z)
					| (0, _, 0) -> (x, y -. 7., z)
					| _ -> (x, y, z -. 7.)
		in
			int_of_float (float_of_int oy +. snd (proj x1 y1 z1) *. float_of_int uy)
;;


(* Simulation graphique du Rubik's cube *)

(* la fonction 'drawPoly' est utilisée pour tracer le pourtour des projections *)
(* des faces des minicubes *)
let drawPoly poly =
	let (x, y) = poly.(0) in Graphics.moveto x y;
		for i = 1 to vect_length poly - 1 do
			let (x, y) = poly.(i) in Graphics.lineto x y
		done;
		let (x, y) = poly.(0) in Graphics.lineto x y;
;;

(* la fonction 'draw' est utilisée pour dessiner la projection 'x' d'une face *)
(* de minicube en superposant le tracé du pourtour à la couleur de remplissage *)
let draw x =
	let a, b = x in
		Graphics.set_color b;
		Graphics.fill_poly a;
		Graphics.set_color Graphics.black;
		drawPoly a
;;

(* la fonction 'draw_face' est utilisée pour dessiner la projection d'une face du *)
(* rubik's cube en appliquant 'draw' aux projections des faces des minicubes *)
(* qu'elle contient. Elle prend en paramètre le résultat de 'proj_face' ci-dessous *)
let rec draw_face l = match l with
		| t :: r -> draw t; draw_face r
		| [] -> ()
;;

(* les trois fonctions suivantes utilisent le repère OVB *)

(* 'face v c' renvoie, si le minicube à l'emplacement d'indice 'c' a une face F *)
(* dans la face du Rubik's cube normale au vecteur sortant 'v', sous forme de vecteur *)
(* une liste circulaire des 4 sommets de F *)

let face v c =
	let e = v /|/ [|1; 1; 1|] in let w = [|e; e; e|] in
			let w1 = w /:/ rot v in
				let w2 = w1 /:/ rot v in
					let w3 = w2 /:/ rot v in
						let l = [w; w1; w2; w3] in
							let add m = for i = 0 to 2 do m.(i) <- m.(i) + !coeff * c.(i) done
							in
								do_list add l;
								vect_of_list l;
;;

(* pour la face 'f' du rubick's cube repérée par le vecteur normal sortant 'v', *)
(* 'face_color' associe à chaque minicube appartenant à cette face 'f' et *)
(* repéré par son indice la couleur qu'il présente sur 'f' et renvoie *)
(* la liste d'associations *)

let face_colors v minicubes =
	let f x = (fst x) /:/ (snd x) in
		map (fun x -> (f x, couleur_de_face (v /:/ transpose (snd x))))
		(select (fun x -> (f x) /|/ v = 1) minicubes)
;;


(* 'face_prepare' fournit, pour la face du rubik's cube normale au vecteur *)
(* sortant 'v', une liste de couples: les couples correspondent aux minicubes *)
(* dont une face F est incluse dans celle du Rubik's cube; la première *)
(* composante est sous forme de vecteur une liste circulaire des 4 sommets *)
(* de F, et la seconde composante est la couleur de F *)

let face_prepare v minicubes =
	let l = face_colors v minicubes in
		map (fun x -> (face v (fst x), snd x)) l
;;


(* on introduit dans les deux fonctions suivantes la matrice de passage p *)
(* du repère ADH au repère du OVB : (x=x'p) où 'x' est la ligne des coordonnées *)
(* ADH et x' la ligne des coordonnées OVB *)

(* 'proj_face' renvoie, comme 'face_prepare', une liste de couples *)
(* 'contour * couleur' correspondant cette fois aux PROJECTIONS *)
(* des faces de minicubes contenues dans la face du Rubik's cube *)
(* orthogonale au vecteur sortant 'v' *)

let proj_face p mv1 v =
	let f = face_prepare v mv1 and
	proj_vect l =
		map_vect (fun x -> xx (v /:/ p) (x /:/ p), yy (v /:/ p) (x /:/ p)) l in
		map (fun x -> proj_vect (fst x), snd x) f
;;
