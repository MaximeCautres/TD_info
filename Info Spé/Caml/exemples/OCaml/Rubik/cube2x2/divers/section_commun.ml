(* dŽcomposition 'm = ker m /*/ sec (sur m)' d'un mouvement 'm' *)
(* avec 'ker m' ŽlŽment du noyau de 'sur' *)
(* 'p' pour 'permutation': 'p = sur m' est la permutation 'p' des indices associŽe au mouvement 'm' *)
let sec p = mv1_of_fun (fun i -> gg i (p i));;
let sur m = fun i -> i /:/ fun_of_mv1 m i;;
let ker m = m /*/ inverse (sec (sur m));;

(* mouvement1 gŽnŽral dŽfini par des permutations et rotations de coins *)
let mv1 pc ec =
	let k = mv1_of_fun
		(fun i -> let x = ec i in if x = 0 then id
								else if x = 1 then st i
								else if x = 2 then transpose (st i)
								else failwith "mv1"
		)
	and l = mv1_of_fun (fun i -> gg i (pc i))
	in k /*/ l
;;
