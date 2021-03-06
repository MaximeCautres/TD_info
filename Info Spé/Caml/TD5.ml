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

(* Parcours en profondeur d'un graphe*)

let dfs (g : graphe) s = 
	let n = Array.length g in let mark = Array.make n true and parcours = ref [] in
	let rec aux (g : graphe) s =
		mark.(s - 1) <- false;
		let fils = associe s g in
		let rec aux1 l = match l with
			|[] -> ()
			|e :: l' -> (if mark.(e - 1) then (aux g e; parcours := e :: !parcours); aux1 l')
		in aux1 fils in 
	aux g s; s :: !parcours;;

dfs model 1;;

(* Parcours en largeur d'un graphes *)

let bfs (g: graphe) s = 
	let n = Array.length g in 
	let states = Array.make n true in 
	let fil = ref [s] in states.(s - 1) <- false;
	let rec aux2 frere = match frere with
		|[] -> ()
		|noeud :: frere' -> (if states.(noeud - 1) then (fil := !fil @ [noeud];
											 states.(noeud - 1) <- false); aux2 frere') in
	let rec aux1 fil = match !fil with
		|[] -> []
		|e :: fil' -> (aux2 (associe e g);  fil := List.tl !fil;  e :: (aux1 fil)) in
	aux1 fil ;;

bfs model 1;;

(* Recherche des composantes connexes d'un graphe*)

let connexe_decomposition (g : graphe) = 
	let n = Array.length g in
	let states = Array.make n true in
	let decompo = ref [] in
	let rec aux l = match l with 
		|[] -> ()
		|e :: l' -> (states.(e - 1) <- false; aux l') in
	for i = 0 to (n - 1) do
		if states.(i) then
			begin
			let connexe = dfs g (i + 1) in
			decompo := !decompo @ [connexe];
			aux connexe end; done;
	!decompo ;;

connexe_decomposition model ;;

(* petit tris fusion pour avoir les composantes dans le bon ordre *)

let rec fusion l1 l2 = match l1, l2 with
	|[], _ -> l2
	|_, [] -> l1
	|n1::l1', n2::l2' -> if n1 <= n2 then n1 :: fusion l1' l2 else n2 :: fusion l1 l2';;

let rec decoupe l l1 l2 = match l with
	|[] -> l1 , l2
	|n::[] -> n :: l1, l2
	|n::m::l' -> decoupe l' (n::l1) (m :: l2);;

let rec tri_fusion l = match l with 
	|[n] -> [n]
	|_ -> let l1, l2 = decoupe l [] [] in
			fusion (tri_fusion l1) (tri_fusion l2);;

(* Les sommets des composantes connexes sont affich�s dans l'ordre croissant *)

let connexe_decomposition_sort (g : graphe) = 
	let n = Array.length g in
	let states = Array.make n true in
	let decompo = ref [] in
	let rec aux l = match l with 
		|[] -> ()
		|e :: l' -> (states.(e - 1) <- false; aux l') in
	for i = 0 to (n - 1) do
		if states.(i) then
			begin
			let connexe = dfs g (i + 1) in
			decompo := !decompo @ [tri_fusion connexe];
			aux connexe end; done;
	!decompo ;;

connexe_decomposition_sort model ;;

(* Recherche d'un cycle dans un graphe *)

let cycle (g : graphe) s = 
	let n = Array.length g in let mark = Array.make n 0 and state = ref 0 in
	mark.(s - 1) <- 1;
	let rec aux (g : graphe) s =
		print_int s;
		let fils = associe s g in
		let rec aux1 l = match l with
			|[] -> ()
			|e :: l' -> (if mark.(e - 1) = 0 && !state = 0 then (aux g e; mark.(e - 1) <- mark.(s) + 1 )
													  else (if mark.(e - 1) <> mark.(s) - 1 then state := !state - 1) 
													  ; aux1 l')
		in aux1 fils in 
	aux g s; !state < 0;;

model ;;

cycle model 1;;


let dfs (g : graphe) s = 
	let n = Array.length g in let mark = Array.make n true and parcours = ref [] in
	let rec aux (g : graphe) s =
		mark.(s - 1) <- false;
		let fils = associe s g in
		let rec aux1 l = match l with
			|[] -> ()
			|e :: l' -> (if mark.(e - 1) then (aux g e; parcours := e :: !parcours); aux1 l')
		in aux1 fils in 
	aux g s; s :: !parcours;;




