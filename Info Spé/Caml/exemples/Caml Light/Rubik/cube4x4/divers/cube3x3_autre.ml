let psd_of_mv1_3 () = (* produit semi direct *)
	let indices () = let l = ref [] in
			for k = 1 downto - 1 do
				for j = 1 downto - 1 do
					for i = 1 downto - 1 do l := [|i; j; k|] :: !l
					done
				done
			done;
			subtract !l [[|0; 0; 0|]]
	and est_angle v =
		let j = ref 0 in
			for i = 0 to 2 do
				if v.(i) <> 0 then incr j
			done;
			!j = 2
	and est_coin v = v.(0) <> 0 && v.(1) <> 0 && v.(2) <> 0
	in
	(* élément neutre de M *)
		let e = map (fun x -> x, id) (indices ())
		in
		(* conversion entre mouvement représenté par une fonction et mouvement *)
		(* représenté par une liste : int vect * int vect vect) list *)
			let mv1_of_fun f =
				map (fun (x, y) -> (x, y /./ (f x))) e
			and fun_of_mv1 (mv1: (int vect * int vect vect) list) x =
				assoc x mv1
			in
			
			(* loi interne *)
				let prefix /*/ mv1 mv1' =
					let f = fun_of_mv1 mv1 and f' = fun_of_mv1 mv1'
					in
						let s t = t /:/ (f t)
						in mv1_of_fun (fun x -> (f x) /./ (f' (s x)))
				and
				
				(* inverse d'un élément *)
				inverse mv1 = map (fun (x, y) -> (x /:/ y, transpose y)) mv1
				
				in
					
					let indice_angle v = let k = ref 0 in
							while v.(!k) <> 0 do incr k done;
							!k
					
					in
					(* renvoie une matrice 'm' de rotation du cube différente de 'id' telle que 'vm=v', où 'v' est un angle *)
						let stabilise_angle v =
							let i = indice_angle v in
								let j = (i + 1) mod 3 and k = (i + 2) mod 3 in
									let m = make_matrix 3 3 0 in
										m.(i).(i) <- - 1;
										m.(j).(k) <- v.(j) * v.(k);
										m.(k).(j) <- v.(j) * v.(k);
										m
										
										
										(* renvoie une matrice 'm' de rotation du cube telle que 'vm=w', avec 'm=id' si 'v=w', où 'v' et 'w' sont des angles *)
						and trans_angle v w =
							let (i, i') = (indice_angle v, indice_angle w) in
								let j = (i + 1) mod 3 and k = (i + 2) mod 3
								and j' = (i' + 1) mod 3 and k' = (i' + 2) mod 3 in
									let a = v.(j) * w.(j') and b = v.(k) * w.(k') in
										let m = make_matrix 3 3 0 in
											m.(j).(j') <- a;
											m.(k).(k') <- b;
											m.(i).(i') <- a * b;
											m
											
											(* matrices de transition d'un coin à un autre *)
											
											(* renvoie une matrice 'm' de rotation du cube différente de 'id' telle que 'vm=v', où 'v' est un coin *)
						and stabilise_coin v =
							let (a, b, c) = vect v in
								let m = [|[|0; a * b; 0|]; [|0; 0; b * c|]; [|c * a; 0; 0|]|] in
									if a * b * c > 0 (* cohérence des sens de rotation *)
									then transpose m
									else m
										
										(* renvoie une matrice 'm' de rotation du cube telle que 'vm=w', avec 'm=id' si 'v=w' , où 'v' et 'w' sont des coins *)
						and trans_coin v w =
							let (a, b, c) = (v.(0) * w.(0), v.(1) * w.(1), v.(2) * w.(2))
							and m = make_matrix 3 3 0 in
								m.(0).(0) <- a;
								if a * b * c = 1 then (
										m.(1).(1) <- b;
										m.(2).(2) <- c
									)
								else (
										m.(2).(1) <- v.(2) * w.(1);
										m.(1).(2) <- v.(1) * w.(2)
									);
								m
						in
							
							let gg i j =
								if est_angle i && est_angle j then trans_angle i j
								else if est_coin i && est_coin j then trans_coin i j
								else id
							in
								let st i = if est_angle i then stabilise_angle i
									else if est_coin i then stabilise_coin i
									else failwith "st"
										
										(* relèvement d'un mouvement 'm' et décomposition 'm = ker m /*/ sec (sur m)' *)
										(* avec 'ker m' élément du noyau de 'sur' *)
										(* p pour 'permutation', par exemple 'p = sur m' *)
								and sec p = mv1_of_fun (fun i -> gg i (p i))
								and sur m = fun i -> i /:/ fun_of_mv1 m i
								in let ker m = m /*/ inverse (sec (sur m))
									in (sec, sur, ker, st)
;;

include "exemples/Caml Light/Rubik/cube4x4/divers/3x3.ml";;