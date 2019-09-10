(* noms des couleurs par défaut des faces *)
let nom_de_face v =
	match vect v with
		| - 1, 0, 0 -> "rouge"
		| 1, 0, 0 -> "orange"
		| 0, - 1, 0 -> "bleu"
		| 0, 1, 0 -> "vert"
		| 0, 0, - 1 -> "jaune"
		| 0, 0, 1 -> "blanc"
		| _ -> "noir"
;;

let face_de_nom c = match c with
		| "orange" -> [|1; 0; 0|]
		| "vert" -> [|0; 1; 0|]
		| "blanc" -> [|0; 0; 1|]
		| "rouge" -> [|- 1; 0; 0|]
		| "bleu" -> [|0; - 1; 0|]
		| "jaune" -> [|0; 0; - 1|]
		| "-" | "" -> [|0; 0; 0|]
		| _ -> failwith "couleur"
;;

let couleur_de_nom nom =
	match nom with
		| "rouge" -> graphics__red
		| "orange" -> graphics__rgb 255 165 0
		| "bleu" -> graphics__rgb 0 150 225
		| "vert" -> graphics__green
		| "jaune" -> graphics__yellow
		| "blanc" -> graphics__white
		| _ -> graphics__black
;;

let couleur_de_face v =
	match vect v with
		| - 1, 0, 0 -> graphics__red
		| 1, 0, 0 -> graphics__rgb 255 165 0 (* orange *)
		| 0, - 1, 0 -> graphics__rgb 0 150 225 (* bleu *)
		| 0, 1, 0 -> graphics__green
		| 0, 0, - 1 -> graphics__yellow
		| 0, 0, 1 -> graphics__white
		| _ -> graphics__black
;;
