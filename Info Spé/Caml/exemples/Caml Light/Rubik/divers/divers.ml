(* quelques fonctions auxiliaires sur les matrices en dimension 3 *)

let vect v = if vect_length v = 3 then (v.(0), v.(1), v.(2))
	else failwith "vect"
;;

let identity a = let m = make_matrix 3 3 0 in
		for i = 0 to 2 do
			m.(i).(i) <- a
		done;
		m
;;

let id = identity 1 and idm = identity (- 1);;

(* produit du vecteur ligne entier v par la matrice entière m *)
let prefix /:/ v m =
	let w j = let t = ref 0 in for k = 0 to vect_length v - 1 do
				t := !t + m.(k).(j) * v.(k) done;
			!t in
		[|w 0; w 1; w 2|]
;;

(* produit matriciel *)
let prefix /./ m m1 = map_vect (fun v -> v /:/ m1) m;;

(* somme matricielle *)
let prefix /+/ m1 m2 =
	let m = make_matrix 3 3 0 in
		for i = 0 to 2 do
			for j = 0 to 2 do
				m.(i).(j) <- m1.(i).(j) + m2.(i).(j)
			done;
		done;
		m
;;

(* produit de la colonne v par la ligne w *)
let prefix /::/ v w =
	let m = make_matrix 3 3 0 in
		for i = 0 to 2 do
			for j = 0 to 2 do
				m.(i).(j) <- v.(i) * w.(j)
			done;
		done;
		m
;;

(* matrice diagonale *)
let diag a b c = [|[|a; 0; 0|]; [|0; b; 0|]; [|0; 0; c|]|];;

(* transposée de la matrice m  qui en est aussi l'inverse : *)
(* quand m est orthogonale *)
let transpose m =
	let m1 = make_matrix 3 3 0 in
		for i = 0 to 2 do
			for j = 0 to 2 do
				m1.(j).(i) <- m.(i).(j)
			done;
		done;
		m1
;;

(* produit scalaire *)
let prefix /|/ v w = v.(0) * w.(0) + v.(1) * w.(1) + v.(2) * w.(2);;

(* matrices des rotations d'un quart de tour autour des axes : *)
(* (opèrent à droite sur les lignes) *)

(* sens des aiguilles d'une montre *)
let rot v = match list_of_vect v with
		| [1; 0; 0] -> [|[|1; 0; 0|]; [|0; 0; - 1|]; [|0; 1; 0|]|]
		| [0; 1; 0] -> [|[|0; 0; 1|]; [|0; 1; 0|]; [|- 1; 0; 0|]|]
		| [0; 0; 1] -> [|[|0; - 1; 0|]; [|1; 0; 0|]; [|0; 0; 1|]|]
		| [- 1; 0; 0] -> [|[|1; 0; 0|]; [|0; 0; 1|]; [|0; - 1; 0|]|]
		| [0; - 1; 0] -> [|[|0; 0; - 1|]; [|0; 1; 0|]; [|1; 0; 0|]|]
		| [0; 0; - 1] -> [|[|0; 1; 0|]; [|- 1; 0; 0|]; [|0; 0; 1|]|]
		| _ -> failwith "rot"
;;

(* sens inverse des aiguilles d'une montre *)
let rot' v = match list_of_vect v with
		| [1; 0; 0] -> [|[|1; 0; 0|]; [|0; 0; 1|]; [|0; - 1; 0|]|]
		| [0; 1; 0] -> [|[|0; 0; - 1|]; [|0; 1; 0|]; [|1; 0; 0|]|]
		| [0; 0; 1] -> [|[|0; 1; 0|]; [|- 1; 0; 0|]; [|0; 0; 1|]|]
		| [- 1; 0; 0] -> [|[|1; 0; 0|]; [|0; 0; - 1|]; [|0; 1; 0|]|]
		| [0; - 1; 0] -> [|[|0; 0; 1|]; [|0; 1; 0|]; [|- 1; 0; 0|]|]
		| [0; 0; - 1] -> [|[|0; - 1; 0|]; [|1; 0; 0|]; [|0; 0; 1|]|]
		| _ -> failwith "rot'"
;;

(* liste dans l'ordre des éléments de l satisfaisant 'critère' *)
let rec select critere l = match l with
		t :: r -> let l1 = select critere r in if critere t then t :: l1 else l1
		| _ -> []
;;

(* liste des entiers de 0 à n - 1 *)
let liste n =
	let v = make_vect n 0 in
		for i = 0 to n - 1 do
			v.(i) <- i
		done;
		list_of_vect v
;;

(* permutation aléatoire des éléments d'une liste l *)
let random_list l =
	let n = list_length l and l1 = ref []
	in
		for i = 0 to n - 1 do
			l1 := (vect_of_list (subtract l !l1)).(random__int (n - i)) :: !l1
		done;
		!l1
;;

(* signature de la permutation p des éléments de la liste l *)
let sign l p =
	let n = list_length l and v = vect_of_list l
	and m = ref 1 in
		for i = 0 to n - 1 do
			for j = i + 1 to n - 1 do
				let a = v.(i) and b = v.(j) in
					if p a > p b && b > a || p b > p a && a > b then m := - !m;
			done;
		done;
		!m
;;

(* exécution d'une liste de mouvements *)
let rec exe l = match l with
		t :: r -> t (); exe r;
		| [] -> ()
;;

(* enregistrement sur disque d'un mouvement *)
let enregistrer mouvement file =
	let chan_out = open_out_bin file in
		output_value chan_out mouvement;
		close_out chan_out
;;

(* lecture sur disque d'un mouvement *)
let lire_mouvement file =
	try
		let chan_in = open_in_bin file
		in
			let mv1_saved =
				input_value chan_in
			in
				close_in chan_in;
				mv1_saved
	with sys__Sys_error s -> failwith s
;;
