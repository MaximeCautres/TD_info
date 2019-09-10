(* dŽcomposition 'm = ker m /*/ sec (sur m)' d'un mouvement 'm' *)
(* avec 'ker m' ŽlŽment du noyau de 'sur' *)
(* 'p' pour 'permutation': 'p = sur m' est la permutation 'p' des indices associŽe au mouvement 'm' *)
let sec p = mv1_of_fun (fun i -> gg i (p i));;
let sur m = fun i -> i /:/ fun_of_mv1 m i;;
let ker m = m /*/ inverse (sec (sur m));;

(* mouvement gŽnŽral de type 'mv1' dŽfini par des permutations et rotations d'angles et de coins et des exposants d'angles et de coins *)
(* avec prise en compte de la rotation des milieux des faces *)
let nouveau_mv1 pa pc ea ec em =
	let k = mv1_of_fun
		(fun i ->
							if est_angle i then if ea i = 0 then id else st i
							else if est_coin i then
								let e = ec i in
									if e = 0 then id
									else if e = 1 then st i
									else transpose (st i)
							else if est_centre i then
								let e = em i in
									if e = 0 then id else if e = 1 then rot i else if e = 2 then rot i /./ rot i else rot' i
							else id
		)
	and l = mv1_of_fun
		(
			fun i ->
							if est_angle i then gg i (pa i) else if est_coin i then gg i (pc i)
							else id
		)
	in k /*/ l
;;
