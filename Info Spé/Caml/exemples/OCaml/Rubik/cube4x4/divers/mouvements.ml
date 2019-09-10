#use "exemples/OCaml/Rubik/cube4x4/divers/indices.ml";;
#use "exemples/OCaml/Rubik/divers/groupe_des_mouvements.ml";;
#use "exemples/OCaml/Rubik/divers/groupe_du_cube.ml";;

(* mouvements de Rubik élémentaires *)

(* rotations dans le sens des aiguilles d'une montre d'un quart de tour de la *)
(* face normale au vecteur sortant v *)

let rub3 v = mv1_of_fun
	(fun x -> if (x /|/ v) = 3 then rot v else id)
;;

(* mouvement inverse du précédent *)
let rub3' v = inverse (rub3 v);;

