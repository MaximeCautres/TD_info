(* indices des coins *)
let indices = let l = ref [] in
		for k = 1 downto - 1 do
			for j = 1 downto - 1 do
				for i = 1 downto - 1 do l := [|i; j; k|] :: !l
				done
			done
		done;
		select (fun x -> x.(0) * x.(1) * x.(2) <> 0) !l
;;
