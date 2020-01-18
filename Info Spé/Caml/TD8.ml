let rec appartient e l = match l with
	|[] -> false
	|x :: l' -> if e = x then true else appartient e l';;

let rec supprime e l = match l with
	|[] -> []
	|x :: l' -> if x = e then supprime e l' else x :: (supprime e l') ;;

let rec ajoute e l = match l with
	|[] -> [e]
	|x :: l' -> if x = e then l else x :: ajoute e l';;

let indice (i, j) = 
	((i / 3) * 3 + j / 3,
	(i mod 3) * 3 + j mod 3);;

indice (5, 3);;

let rec copy l = match l with
	|[] -> []
	|x :: l' -> x :: (copy l');;

let rec deepcopy l = match l with
	|[] -> []
	|il :: l' -> (copy l) :: deepcopy l';;

type litteral = X of int * int* int | NonX of int * int * int;;

let creer_litteral i j k state = 
	if state then X(i, j, k) else NonX(i, j, k);;

creer_litteral 2 3 6 true;;

let case1 ()= 
	let l = ref [] in 
	for n = 0 to 80 do
		let l' = ref [] in
		for k = 1 to 9 do
			l' := (creer_litteral (n / 9) (n mod 9) k true) :: !l'
			done;
		l := !l' :: !l
		done;
	!l;;



let bloc11 ()= 
	let l = ref [] in 
	for i = 0 to 8 do
		for k = 1 to 9 do 
			let l' = ref [] in
			for j = 0 to 0 do
			let (x, y) = indice (i, j) in
				l' := (creer_litteral x y k true) :: !l'
				done;
			l := !l' :: !l
			done;
		done;
	!l;;

let lig2 ()= 
	let l = ref [] in
	for i = 0 to 8 do
		for k = 1 to 9 do
			for j = 0 to 7 do
				for j' = j + 1 to 8 do
					l := (creer_litteral i j k false, creer_litteral i j' k false) :: !l
					done;
				done;
			done;
		done;
	!l;;

List.length (lig2 ());;


let donnees tabl = 
	let f2 = ref [] in
	for i = 0 to 8 do 
		for j = 0 to 8 do
			if table.[i].[j] > 0 then
				begin
				f2 := (creer_litteral i j table.[i].[j] true) :: !f2
				for k = 1 to 9 do
					if k <> table.[i].[j]  then f2 := (creer_litteral i j k false) :: !f2
					done;
				end
			done;
		done;
	!f2;;
