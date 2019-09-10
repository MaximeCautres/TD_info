(* indices des angles et des coins et des centres des faces *)
let indices = let l = ref [] in
		for k = 1 downto - 1 do
			for j = 1 downto - 1 do
				for i = 1 downto - 1 do l := [|i; j; k|] :: !l
				done
			done
		done;
		subtract !l [[|0; 0; 0|]]
;;
