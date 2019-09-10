let psd_of_mv1_3 () = (* produit semi direct *)
	let indices () = let l = ref [] in
			for k = 1 downto - 1 do
				for j = 1 downto - 1 do
					for i = 1 downto - 1 do l := [|i; j; k|] :: !l
					done
				done
			done;
			subtract !l [[|0; 0; 0|]]
	and est_angle x = x /|/ x = 2
	and est_coin x = x /|/ x = 3
	in
		let marque x =
			if est_coin x then [|0; 0; x.(2)|] else if est_angle x then
				let a, b, c = x.(0), x.(1), x.(2)
				in match a, b, c with
						| 0, _, _ -> [|0; b; 0|]
						| _, 0, _ -> [|0; 0; c|]
						| _, _, 0 -> [|a; 0; 0|]
						| _ -> [|0; 0; 0|]
			else [|0; 0; 0|]
		in
			let gg i j =
				try
					let critere i j g = i /:/ g = j && marque i /:/ g = marque j
					in
						hd (select (critere i j) groupe_du_cube)
				with Failure "hd" -> id
								(* cette liste devrait toujours contenir exactement un élément *)
			in
				let stc i =
					let x = [|1; 1; 1|]
					and m = [|[|0; 0; 1|]; [|1; 0; 0|]; [|0; 1; 0|]|]
					in
						gg i x /./ m /./ gg x i
				
				and sta i = let x = [|1; 0; 1|]
					and m = [|[|0; 0; 1|]; [|0; - 1; 0|]; [|1; 0; 0|]|]
					in
						gg i x /./ m /./ gg x i
				in
					let st i = if est_angle i then sta i
						else if est_coin i then stc i
						else failwith "st"
					and mv1_of_fun f =
						map (fun x -> (x, f x)) (indices ())
					and fun_of_mv1 mv1 x =
						assoc x mv1
					
					in
						let prefix /*/ mv1 mv1' =
							let f = fun_of_mv1 mv1 and f' = fun_of_mv1 mv1'
							in
								let s t = t /:/ (f t)
								in mv1_of_fun (fun x -> (f x) /./ (f' (s x)))
						and inverse mv1 = map (fun (x, y) -> (x /:/ y, transpose y)) mv1
						in
						(* relèvement d'un mouvement 'm' et décomposition 'm = ker m /*/ sec (sur m)' *)
						(* avec ker m élément du noyau de 'sur' *)
						(* 'p' pour 'permutation', par exemple 'p = sur m' *)
							let sec p = mv1_of_fun (fun i -> gg i (p i))
							and sur m = fun i -> i /:/ fun_of_mv1 m i
							in let ker m = m /*/ inverse (sec (sur m))
								in (sec, sur, ker, st)
;;

include "exemples/Caml Light/Rubik/cube4x4/divers/3x3.ml";;
