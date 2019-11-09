(* Partie 1*)

(* <Partie 2 *)

type graphe = (int * int list) array ;;

let model = [| 1, [2;5]; 2,[1;3;5] ; 3 , [2;4] ; 4 , [3;5;6] ; 5 , [1;2;4] ; 6 , [4]; 7,[8] ; 8,[7]|]
;;
let sommet_max l = match l with
	|[] -> failwith "graphe vide"
	|branche :: l' -> (
	let rec aux1 l m = match l with
		|[] -> m
		|e :: l' -> let a, b = e in aux1 l' (max (max m a) b) in
let a, b = branche in aux1 l' (max a b));;

sommet_max [(1,2); (1, 3)];;

let matrice_adj l = 
	let n = sommet_max l in
	let matrice = Array.make_matrix (n + 1) (n + 1) false in
	let rec aux l = match l with
		|[] -> ()
		|e :: l' -> (let a, b = e in matrice.(a).(b)<- true; matrice.(b).(a) <- true; aux l')
	in aux l, matrice;;

matrice_adj [(1,2); (1, 3)];;

let liste_adj l = 
	let n = sommet_max l in 
	let z = Array.make n (0, []) in
	for i = 0 to (n - 1) do 
		z.(i) <- (i+1, [])
	done;
	let rec aux l = match l with
		|[] -> ()
		|e :: l' -> (let a, b = e in let k, former = z.(a - 1) in z.(a - 1) <- (k, b::former);
											  let k, former = z.(b - 1) in z.(b - 1) <- (k, a::former); 
											  aux l')
	in aux l; z;;

liste_adj [(1,2); (1, 3)];;

let associe i (g : graphe) = let a, b = g.(i-1) in b;;

associe 1 [|(1, [3; 2]); (2, [1]); (3, [1])|] ;;

let dfs (g : graphe) s = 
	let n = Array.length g in let mark = Array.make n true and parcours = ref [] in
	let rec aux (g : graphe) s =
		print_int s;
		mark.(s - 1) <- false;
		let fils = associe s g in
		let rec aux1 l = match l with
			|[] -> ()
			|e :: l' -> (if mark.(e - 1) then (aux g e; parcours := e :: !parcours); aux1 l')
		in aux1 fils in 
	aux g s; s :: !parcours;;

dfs model 1;;

let bfs (g: graphe) s =

