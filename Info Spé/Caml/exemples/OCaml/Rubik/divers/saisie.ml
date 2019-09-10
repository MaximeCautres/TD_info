(*- boutons de saisie d'une configuration du cube à partir des couleurs des faces visibles des minicubes -*)
(* chaque face visible de minicube devient un bouton de type 'btn' *)
(* aucune vérification n'est faite en cours de saisie *)

(* dessin d'une face de minicube selon la couleur choisie *)
let dessine_face face couleur =
	let vct v = (v.(0), v.(1))
	in
		let poly = map_vect vct face in
			Graphics.set_color couleur;
			Graphics.fill_poly poly;
			Graphics.set_color Graphics.black;
			let (x, y) = poly.(0) in Graphics.moveto x y;
				for i = 1 to vect_length poly - 1 do
					let (x, y) = poly.(i) in Graphics.lineto x y
				done;
				let (x, y) = poly.(0) in Graphics.lineto x y
;;

(* dessin des projections des boutons de type 'btn' recensés dans le vecteur de boutons 'btn_vect' *)
let dessine_btns btn_vect =
	let dessine_faces btn =
		for i = 0 to vect_length btn.btn_faces - 1 do
			dessine_face btn.btn_faces.(i) (couleur_de_nom btn.btn_couleurs.(i))
		done
	in
		for i = 0 to vect_length btn_vect - 1 do
			dessine_faces btn_vect.(i);
		done
;;

(* renvoie 'true' ssi le point 'x' est intérieur au contour 'p' *)
let est_dans p x =
	let (/-/) v1 v2 = let v = make_vect 3 0 in
			for i = 0 to 1 do
				v.(i) <- v1.(i) - v2.(i)
			done;
			v
	and det v1 v2 = v1.(0) * v2.(1) - v1.(1) * v2.(0)
	in
		let p0 = p.(0) /-/ x
		and p1 = p.(1) /-/ x
		and p2 = p.(2) /-/ x
		and p3 = p.(3) /-/ x
		in
			det p0 p1 * det p1 p2 > 0
			&& det p1 p2 * det p2 p3 > 0
			&& det p2 p3 * det p3 p0 > 0
;;

(* couleur utilisée pour colorier une face quand on clique dessus *)
(* cette couleur est choisie à l'aide des 6 boutons de choix de couleur ci dessous *)
let couleur_choisie = ref "orange";;

(* fonction de gestion des boutons de type 'btn' utilisés pour colorier les faces *)
(* cette fonction est appelée par la boucle principale 'boucle_cube()' ci-dessous *)
let gestion_btns mousex mousey =
	let btn_action btn mousex mousey =
		for i = 0 to vect_length btn.btn_faces - 1 do
			let face = btn.btn_faces.(i) in
				if est_dans face [|mousex; mousey|] then (
						btn.btn_couleurs.(i) <- !couleur_choisie;
						dessine_face face (couleur_de_nom btn.btn_couleurs.(i))
					)
		done
	in
		for i = 0 to vect_length btn_vect - 1 do
			btn_action btn_vect.(i) mousex mousey
		done
;;

(*- fin de boutons de saisie d'une configuration du cube -*)


(*- rangée des boutons de choix de couleurs -*)

let boutons_couleurs_vect =
	let titres =
		[|"orange"; "rouge"; "vert"; "bleu"; "blanc"; "jaune"|]
	in
		let n = vect_length titres
		in
			let boutons = make_vect n (bouton "" 0 0 0 0 0 (fun () -> ()))
			in
				for i = 0 to n - 1 do
					boutons.(i) <- {titre = titres.(i); orx = i * 50; ory = 0; hauteur = 30;
						largeur = 50; couleur = couleur_de_nom titres.(i); action = fun () -> ()}
				done;
				boutons
;;

let action_bouton_couleur bouton_couleur =
	couleur_choisie := bouton_couleur.titre;
;;

let set_actions_boutons_couleurs () =
	for i = 0 to vect_length boutons_couleurs_vect - 1 do
		boutons_couleurs_vect.(i).action <- fun () -> action_bouton_couleur boutons_couleurs_vect.(i)
	done
;;

let dessine_boutons_couleurs () =
	for i = 0 to vect_length boutons_couleurs_vect - 1 do
		dessine_bouton boutons_couleurs_vect.(i)
	done
;;

(* fonction de gestion des boutons de choix de la couleur utilisée pour colorier les faces *)
(* cette fonction est appelée par la boucle principale 'boucle_saisie' ci-dessous *)
let gestion_boutons_couleurs mousex mousey =
	for i = 0 to vect_length boutons_couleurs_vect - 1 do
		gestion_bouton boutons_couleurs_vect.(i) mousex mousey
	done;
;;


(*- fin de rangée des boutons de choix de couleurs -*)

exception Quitter;;

let boucle_saisie s =
	Graphics.open_graph s;
	dessine_btns btn_vect;
	dessine_boutons_couleurs ();
	set_actions_boutons_couleurs ();
	try
		let largeur1, hauteur1 = Graphics.text_size "Quitter"
		in
			let bouton_quitter = bouton "Quitter" (Graphics.size_x () - largeur1 - 20) (Graphics.size_y () - hauteur1 - 20)
				(largeur1 + 10) (hauteur1 + 10) Graphics.yellow (fun () -> raise Quitter)
			in
				dessine_bouton bouton_quitter;
				while true do
					let status = Graphics.wait_next_event [Graphics.Button_down] in
						let mousex = status.Graphics.mouse_x and mousey = status.Graphics.mouse_y
						in
							if status.Graphics.button then (
									gestion_bouton bouton_quitter mousex mousey;
									gestion_btns mousex mousey;
									gestion_boutons_couleurs mousex mousey;
									(*printf__printf "\nmouse_x = %d , mouse_y = %d \n" mousex mousey;*)
									(*print_newline ()*)
								)
				done;
	with Quitter -> Graphics.close_graph ()
;;
