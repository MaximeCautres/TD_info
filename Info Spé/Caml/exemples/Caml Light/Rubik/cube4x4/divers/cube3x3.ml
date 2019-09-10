let psd_of_mv1_3 () = (* produit semi direct *)
	let indices = let l = ref [] in
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
		let gg i j =
			let gk i =
				let gka j = match vect j with
						| (0, 1, 1) (* kappa *) -> id
						| (1, 0, 1) -> rot [|0; 0; 1|] (* gamma *)
						| (0, - 1, 1) -> [|[|- 1; 0; 0|]; [|0; - 1; 0|]; [|0; 0; 1|]|] (* gamma^2 *)
						| (- 1, 0, 1) -> rot' [|0; 0; 1|] (* gamma^3 *)
						
						| (0, 1, - 1) -> rot [|1; 0; 0|]
						| (0, - 1, - 1) -> [|[|1; 0; 0|]; [|0; - 1; 0|]; [|0; 0; - 1|]|]
						| (- 1, 0, - 1) -> [|[|0; - 1; 0|]; [|- 1; 0; 0|]; [|0; 0; - 1|]|]
						| (1, 0, - 1) -> [|[|0; 1; 0|]; [|1; 0; 0|]; [|0; 0; - 1|]|]
						| (1, 1, 0) -> rot' [|0; 1; 0|]
						| (1, - 1, 0) -> [|[|0; 0; 1|]; [|0; - 1; 0|]; [|1; 0; 0|]|]
						| (- 1, - 1, 0) -> [|[|0; 0; - 1|]; [|0; - 1; 0|]; [|- 1; 0; 0|]|]
						| (- 1, 1, 0) -> rot [|0; 1; 0|]
						| _ -> failwith "gka"
				and gkc j = match vect j with
						| (1, 1, 1) (* kappa *) -> id
						| (1, - 1, 1) -> rot [|0; 0; 1|] (* gamma *)
						| (- 1, - 1, 1) -> [|[|- 1; 0; 0|]; [|0; - 1; 0|]; [|0; 0; 1|]|] (* gamma^2 *)
						| (- 1, 1, 1) -> rot' [|0; 0; 1|] (* gamma^3 *)
						
						| (- 1, - 1, - 1) ->
									[|[|0; 0; - 1|]; [|0; - 1; 0|]; [|- 1; 0; 0|]|] (* demi-tour / (1,0,-1) *)
						| (1, 1, - 1) -> rot [|1; 0; 0|]
						| (1, - 1, - 1) -> [|[|1; 0; 0|]; [|0; - 1; 0|]; [|0; 0; - 1|]|]
						| (- 1, 1, - 1) -> [|[|- 1; 0; 0|]; [|0; 1; 0|]; [|0; 0; - 1|]|]
						| _ -> failwith "gkc"
				in
					if est_angle i then gka i else if est_coin i then gkc i else id
			in
				transpose (gk i) /./ gk j
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
					map (fun x -> (x, f x)) indices
				and fun_of_mv1 (mv1: (int vect * int vect vect) list) x =
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
