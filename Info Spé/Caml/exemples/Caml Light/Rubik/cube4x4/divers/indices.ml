let indices =
	let l = ref [] in
		for i = 0 to 3 do
			for j = 0 to 3 do
				for k = 0 to 3 do
					l := [|2 * i - 3; 2 * j - 3; 2 * k - 3|] :: !l
				done
			done
		done;
		let f t = if t < 0 then - t else t in
			list_of_vect (identity 3) @ list_of_vect (identity (- 3)) @
			select (fun t -> f t.(0) > 1 || f t.(1) > 1 || f t.(2) > 1) !l
;;
