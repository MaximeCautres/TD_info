include "exemples/Caml Light/Rubik/divers/boutons.ml";;


let dessine_boutons1 cube =
	let couleur_titre titre =
		let face titre = match titre with
				| "A" | "A'" | "ai" | "ai'" | "a" | "a'" -> [|1; 0; 0|]
				| "D" | "D'" | "di" | "di'" | "d" | "d'" -> [|0; 1; 0|]
				| "H" | "H'" | "hi" | "hi'" | "h" | "h'" -> [|0; 0; 1|]
				| "pi" | "pi'" -> [|- 1; 0; 0|]
				| "gi" | "gi'" -> [|0; - 1; 0|]
				| "bi" | "bi'" -> [|0; 0; - 1|]
				| "p" | "p'" -> [|- 1; 0; 0|]
				| "g" | "g'" -> [|0; - 1; 0|]
				| "b" | "b'" -> [|0; 0; - 1|]
				| _ -> print_string titre; failwith "face"
		in
			couleur_de_face ((face titre) /:/ transpose cube.context1.matrice)
	in
		for i = 0 to vect_length cube.boutons1 - 1 do
			cube.boutons1.(i).couleur <- couleur_titre cube.boutons1.(i).titre;
			dessine_bouton cube.boutons1.(i)
		done
;;

let gestion_boutons1 cube mousex mousey =
	for i = 0 to vect_length cube.boutons1 - 1 do
		gestion_bouton cube.boutons1.(i) mousex mousey
	done;
	dessine_boutons1 cube
;;

let cree_boutons1 cube =
	
	let titres =
		[|"A"; "A'"; "H"; "H'"; "D"; "D'";
			"a"; "a'"; "h"; "h'"; "d"; "d'";
			"p"; "p'"; "b"; "b'"; "g"; "g'"
		|]
	in
		let set_actions boutons =
			let (OPS (a0, d0, h0), OPS (a0', d0', h0')) = cube.op_globales1
			and (OPS (a, d, h), OPS (a', d', h'), OPS (p, g, b), OPS (p', g', b')) =
				cube.op_externes1
			in
				let v = [|a0; a0'; h0; h0'; d0; d0'; a; a'; h; h'; d; d'; p; p'; b; b'; g; g'|]
				in
					for i = 0 to vect_length v - 1 do
						set_action boutons.(i) v.(i)
					done
		
		in
			let n = vect_length titres in
				let boutons = make_vect n (bouton "" 0 0 0 0 0 (fun () -> ())) in
					for i = 0 to n - 1 do
						boutons.(i) <- {titre = titres.(i); orx = i * 37; ory = 0; hauteur = 30;
							largeur = 37; couleur = graphics__white; action = fun () -> ()}
					done;
					set_actions boutons;
					cube.boutons1 <- boutons;
;;

let cree_boutons1i cube =
	
	let titres =
		[|
			"ai"; "ai'"; "hi"; "hi'"; "di"; "di'";
			"pi"; "pi'"; "bi"; "bi'"; "gi"; "gi'"
		|]
	in
		let set_actions boutons =
			let (OPS (a, d, h), OPS (a', d', h'), OPS (p, g, b), OPS (p', g', b')) =
				cube.op_externes1i
			in
				let v = [|a; a'; h; h'; d; d'; p; p'; b; b'; g; g'|]
				in
					for i = 0 to vect_length v - 1 do
						set_action boutons.(i) v.(i)
					done
		
		in
			let n = vect_length titres in
				let boutons = make_vect n (bouton "" 0 0 0 0 0 (fun () -> ())) in
					for i = 0 to n - 1 do
						boutons.(i) <- {titre = titres.(i); orx = (i + 6) * 37; ory = 30; hauteur = 30;
							largeur = 37; couleur = graphics__white; action = fun () -> ()}
					done;
					set_actions boutons;
					cube.boutons1 <- vect_of_list (list_of_vect cube.boutons1 @ list_of_vect boutons);
;;

exception Quitter;;

let boucle1 cube action1 action2 action3 action4 action5 =
	graphics__open_graph " 666x800";
	dessine_cube cube.context1 cube.mouvement1.mv1;
	
	cree_boutons1 cube;
	cree_boutons1i cube;
	try
		let largeur, hauteur = graphics__text_size "centres"
		and largeur4, hauteur4 = graphics__text_size "angles"
		and largeur5, hauteur5 = graphics__text_size "Résolution simple"
		and largeur6, hauteur6 = graphics__text_size "Résolution complète"
		and largeur1, hauteur1 = graphics__text_size "Quitter"
		and largeur2, hauteur2 = graphics__text_size "Mélanger"
		
		in
			let bouton_centres = bouton "Centres" 20 (graphics__size_y () - hauteur - 20)
				(largeur + 10) (hauteur + 10) graphics__yellow action1
			and bouton_angles = bouton "Angles" 20 (graphics__size_y () - 2 * (hauteur4 + 20))
				(largeur4 + 10) (hauteur4 + 10) graphics__yellow action2
			and bouton_cube3 = bouton "Résolution simple" 20 (graphics__size_y () - 3 * (hauteur4 + 20))
				(largeur5 + 10) (hauteur5 + 10) graphics__yellow action3
			and bouton_cube4 = bouton "Résolution complète" 20 (graphics__size_y () - 4 * (hauteur4 + 20))
				(largeur6 + 10) (hauteur6 + 10) graphics__yellow action4
			and bouton_melanger = bouton "Mélanger" ((graphics__size_x () - largeur2) / 2 - 10) (graphics__size_y () - hauteur2 - 20)
				(largeur2 + 10) (hauteur2 + 10) graphics__yellow action5
			and bouton_quitter = bouton "Quitter" (graphics__size_x () - largeur1 - 20) (graphics__size_y () - hauteur1 - 20)
				(largeur1 + 10) (hauteur1 + 10) graphics__yellow (fun () -> raise Quitter)
			
			in
				dessine_bouton bouton_centres;
				dessine_bouton bouton_angles;
				dessine_bouton bouton_cube3;
				dessine_bouton bouton_cube4;
				dessine_bouton bouton_melanger;
				dessine_bouton bouton_quitter;
				dessine_boutons1 cube;
				while true do
					let status = graphics__wait_next_event [graphics__Button_down] in
						let mousex = status.graphics__mouse_x and mousey = status.graphics__mouse_y
						in
							if status.graphics__button then (
									gestion_bouton bouton_centres mousex mousey;
									gestion_bouton bouton_angles mousex mousey;
									gestion_bouton bouton_cube3 mousex mousey;
									gestion_bouton bouton_cube4 mousex mousey;
									gestion_bouton bouton_melanger mousex mousey;
									gestion_bouton bouton_quitter mousex mousey;
									gestion_boutons1 cube mousex mousey;
								)
				done;
	with Quitter -> graphics__close_graph ()
;;
