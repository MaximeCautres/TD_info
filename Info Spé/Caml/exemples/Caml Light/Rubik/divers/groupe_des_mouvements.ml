(* groupe M des mouvements des minicubes *)

(* élément neutre de M *)
let e = map (fun x -> x, id) indices;;

(* conversion entre mouvement représenté par une fonction et mouvement *)
(* représenté par une liste : (int vect * int vect vect) list *)
let mv1_of_fun f =
	map (fun (x, y) -> (x, y /./ (f x))) e
;;
let fun_of_mv1 mv1 x =
	assoc x mv1
;;

(* loi interne *)
let prefix /*/ mv1 mv1' =
	let f = fun_of_mv1 mv1 and f' = fun_of_mv1 mv1'
	in
		let s t = t /:/ (f t)
		in mv1_of_fun (fun x -> (f x) /./ (f' (s x)))
;;

(* inverse d'un élément *)
let inverse mv1 = map (fun (x, y) -> (x /:/ y, transpose y)) mv1;;

(* mouvements de Rubik élémentaires *)

(* rotations dans le sens des aiguilles d'une montre d'un quart de tour de la *)
(* face - tranche interne dans le cas du cube 4x4 - normale au vecteur sortant 'v' *)
let rub v = mv1_of_fun
	(fun x -> if (x /|/ v) = 1 then rot v else id)
;;

(* mouvement inverse du précédent *)
let rub' v = inverse (rub v);;
