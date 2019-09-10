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

#load "graphics.cma";;

let couleur_de_nom nom =
	match nom with
		| "rouge" -> Graphics.red
		| "orange" -> Graphics.rgb 255 165 0
		| "bleu" -> Graphics.rgb 0 150 225
		| "vert" -> Graphics.green
		| "jaune" -> Graphics.yellow
		| "blanc" -> Graphics.white
		| _ -> Graphics.black
;;

let couleur_de_face v =
	match vect v with
		| - 1, 0, 0 -> Graphics.red
		| 1, 0, 0 -> Graphics.rgb 255 165 0 (* orange *)
		| 0, - 1, 0 -> Graphics.rgb 0 150 225 (* bleu *)
		| 0, 1, 0 -> Graphics.green
		| 0, 0, - 1 -> Graphics.yellow
		| 0, 0, 1 -> Graphics.white
		| _ -> Graphics.black
;;
