(* Gestion par boutons des mouvements globaux et des mouvements de Rubik *)

let bouton titre orx ory largeur hauteur couleur action =
	{titre = titre; orx = orx; ory = ory; hauteur = hauteur; largeur = largeur;
		couleur = couleur; action = action}
;;

let inverse_bouton b =
	Graphics.set_color Graphics.black;
	Graphics.fill_rect b.orx b.ory b.largeur b.hauteur;
	let (x, y) = Graphics.text_size b.titre in
		Graphics.moveto (b.orx + (b.largeur - x) / 2) (b.ory + (b.hauteur - y) / 2);
		Graphics.set_color Graphics.white;
		Graphics.draw_string b.titre;
		let p = make_vect 4 (0, 0) in
			p.(0) <- (b.orx, b.ory);
			p.(1) <- (b.orx + b.largeur, b.ory);
			p.(2) <- (b.orx + b.largeur, b.ory + b.hauteur);
			p.(3) <- (b.orx, b.ory + b.hauteur);
			Graphics.set_color Graphics.black;
			drawPoly p
;;

let dessine_bouton b =
	Graphics.set_color b.couleur;
	Graphics.fill_rect b.orx b.ory b.largeur b.hauteur;
	let (x, y) = Graphics.text_size b.titre in
		Graphics.moveto (b.orx + (b.largeur - x) / 2) (b.ory + (b.hauteur - y) / 2);
		Graphics.set_color Graphics.black;
		Graphics.draw_string b.titre;
		let p = make_vect 4 (0, 0) in
			p.(0) <- (b.orx, b.ory);
			p.(1) <- (b.orx + b.largeur, b.ory);
			p.(2) <- (b.orx + b.largeur, b.ory + b.hauteur);
			p.(3) <- (b.orx, b.ory + b.hauteur);
			Graphics.set_color Graphics.black;
			drawPoly p
;;

let set_action bouton action =
	bouton.action <- action
;;

let gestion_bouton bouton mousex mousey =
	if bouton.orx < mousex && mousex < bouton.orx + bouton.largeur
		&& bouton.ory < mousey && mousey < bouton.ory + bouton.hauteur then (
			inverse_bouton bouton;
			let t1 = Sys.time () in
				let t2 = ref t1 in
					while !t2 < t1 +. 0.2 do
						t2 := Sys.time ()
					done;
					dessine_bouton bouton;
					bouton.action ();
		);
;;
