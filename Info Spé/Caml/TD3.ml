type 'a file = {mutable debut : 'a list; mutable fin : 'a list};;

let file_vide = {debut = [] ; fin = []};;

let est_vide f = f.debut = [] && f.fin = [] ;;

let rec premier f = match f.debut with
	|[] -> premier {debut = List.rev f.fin; fin = []}
	|n::_ -> n;;

let rec enfile n f = f.fin <- n::f.fin; f;;

let rec defile f = match f.debut with
	|[] -> defile {debut = List.rev f.fin; fin = []}
	|n::f' -> {debut = f'; fin = f.fin};;

file_vide ;;
est_vide {debut = [2]; fin = []};;
premier {debut = [2]; fin = []};;
premier {debut = []; fin = [3;2]};;
enfile 3 {debut = []; fin = [3;2]};;
defile {debut = [3;4]; fin = [3; 3; 2]};;

type 'a fila = {mutable donnees : 'a array ; est_vide : bool ; mutable deb : 'a ; mutable fn : 'a};;

let vile_vide f = f.est_vide;;
let file_vide x = {donnees = (Array.make x 0); est_vide = true; deb = 0; fn = 0};;

let premier f = if f.est_vide then failwith "file vide" 
	else (f.donnees).(f.deb);;

let enfile x f = if f.fn <> f.deb || f.est_vide 
	then ((f.donnees).(f.fn)<-x; f.fn <- (f.fn + 1) mod (Array.length f.donnees); f.est_vide <- true; f)
	else failwith "full file" ;;

let defile f = f.deb <- f.deb + 1 mod (Array.length f.donnees); f.est_vide <- f.deb = f.fn; f;;

(* type tas *)

type tas = Vide | Noeud of int * tas * tas ;;

let min_tas t = match t with
   |Vide -> failwith "tas vide" 
   |Noeud(n,_ ,_) -> n ;;

let rec enleve_min t = match t with
	|Vide -> Vide
	|Noeud(_,g,d)-> match g,d with
		|Noeud(g',_,_),Noeud(d',_,_) -> if g' < d' 
												 then Noeud(g', enleve_min g, d)
												 else Noeud(d', g, enleve_min d)
		|Vide, d -> d
		|g, Vide -> g;;

enleve_min (Noeud(1,Noeud(2,Noeud(4,Vide,Vide),Noeud(5,Vide,Vide)),Noeud(3,Vide,Vide)));;

let rec ajout n t = match t with
	|Vide -> Noeud(n,Vide,Vide)
	|Noeud(x,g,d)-> if x < n 
							then if Random.bool () 
							then Noeud(x, g, ajout n d)
							else Noeud(x, ajout n g, d)
							else if Random.bool ()
							then Noeud(n,Vide,Noeud(x,g,d))
							else Noeud(n,Noeud(x,g,d),Vide);;

let rec ajout_list l = match l with
   |[] ->Vide
   |n::l' -> ajout n (ajout_list l');;

let rec vider t = match t with
	|Vide -> []
	|_ -> (min_tas t)::(vider (enleve_min t));;

let tri_par_tas l = vider (ajout_list l);;

tri_par_tas [1;3;4;2;5;4;3;2];;

let indice c = if c = ' ' then 0 else int_of_char c - 96;;

let frequence s =
	let table = Array.make 27 0 and n = ref 0 in
	for i = 0 to String.length s - 1 do 
		(if table.(indice s.[i]) = 0 then n := !n + 1;
		table.(indice s.[i]) <- (table.(indice s.[i]) + 1))
		done; 
	let caracters = Array.make !n ' ' and iter = Array.make !n 0 
	and c = ref 0 in 
	for i = 0 to 26 do
		if table.(i) != 0 
		then (if i = 0 then caracters.(!c) <- ' ' 
							else caracters.(!c) <- char_of_int (i + 96);
				iter.(!c)<-table.(i);
				c := !c + 1)
	done;
	!n, caracters, iter;;

frequence "elle a mal a la main";;

(* normale apres ce genre d'horreur d'avor mal a la main aussi *);;

let deux_plus_petit_v l = 
	let rec deux_plus_petit l = match l with
	|m1::m2::[] -> if m1 = m2 then failwith "liste une seule valeur" else (m1, m2)
	|m1::m2::n::l' -> if n <= m1 
							then (if n = m1 then deux_plus_petit (m1::m2::l') else deux_plus_petit (n::m1::l'))
							else ( if n <= m2 
									 then deux_plus_petit (m1::n::l')
									 else deux_plus_petit (m1::m2::l'))
	in match l with
	|[] -> failwith " liste vide "
	|m1::[] -> failwith "liste de une seule valeur"
	|m1::m2::l' -> if m1 < m2 then deux_plus_petit l else deux_plus_petit (m2::m1::l');;

(*elle est belle mais c'est absolument pas ce qu'il fallait faire, terrible coup dure *)

let deux_plus_petit_t l = 
	let l = Array.to_list l in
	let rec deux_plus_petit l = match l with
	|m1::m2::[] -> (m1, m2)
	|m1::m2::n::l' -> if n <= m1 
							then  deux_plus_petit (n::m1::l')
							else ( if n <= m2 
									 then deux_plus_petit (m1::n::l')
									 else deux_plus_petit (m1::m2::l'))
	in 
	let inverse l = match l with
	|[] -> failwith " liste vide "
	|m1::[] -> failwith "liste de une seule valeur"
	|m1::m2::l' -> if m1 > m2 then m2::m1::l' else l
	in let l = inverse l in
	let (i, j) = deux_plus_petit l in
	let rec find_i l n = match l with
	|[] -> failwith "n'est pas dans la liste"
	|x::l' -> if x = n then 0 else 1 + (find_i l' n)
	in let i1 = find_i l i in
	let l = Array.of_list l in
	l.(i1) <- max_int;
	let l = Array.to_list l in
	let i2 = find_i l j in
	(i1, i2);;

let deux_plus_petit_i table = let m1 = ref max_int and m2 = ref max_int and 
									i1 = ref 0 and i2 = ref 0 in 
		for i = 0 to (Array.length table - 1) do
			if table.(i) <= !m1 
				then (m2 := !m1; m1 := table.(i); i2 := !i1; i1 := i)
				else ( if table.(i) <= !m2 
							then (m2 := table.(i); i2 := i)
							else ())
		done; (!i1,!i2);;

deux_plus_petit_t [|3;3;1;2|];;

type arbre_huffman = Feuille of char | Noeud of arbre_huffman * arbre_huffman ;;

let rec affiche abr = match abr with
	|Feuille(c) -> print_char c
	|Noeud(g,d) -> (print_char '('; affiche g ; print_char '.' ; affiche d ; print_char ')' );;

affiche (Noeud(Noeud(Feuille('c'),Feuille('d')),Noeud(Feuille('j'),Noeud(Feuille('k'),Feuille('l'))))) ;;


let construit_arbre phrase = 
	let (n, u, v) = frequence phrase in 
	let w = Array.make n (Feuille ' ' ) in
	for i = 0 to n - 1 do
		w.(i) <- Feuille(u.(i))
		done;
	for x = 0 to n-2 do
		let (i, j) = deux_plus_petit_i v in
		v.(i) <- v.(i) + v.(j);
		w.(i) <- Noeud(w.(i),w.(j));
		for k = j+1 to n-1-x do
			v.(k - 1) <- v.(k);
			w.(k - 1) <- w.(k);
		done;
		v.(n-1-x) <- max_int
	done;
	w.(0);;

affiche (construit_arbre "elle a mal a la main");;

type 'a arbre = End of 'a | Cons of 'a arbre * 'a * 'a arbre;;

let rec list abr = match abr with
	|Feuille(c) -> [c]
	|Noeud(g,d) -> ['('] @ (list g) @ ['.'] @ (list d) @ [')'];;

let paths sentences = 
	let rec aux1 sentence c acc l = match sentence with
		|[] -> l
		|ch::sentence' -> match ch with
			|'(' -> aux1 sentence' (c + 1) (acc ^ "0") l
			|')' -> aux1 sentence' (c - 1) (String.sub acc 0 c) l
			|'.' -> aux1 sentence' c ((String.sub acc 0 (c - 2)) ^ "1" ) l 
			|_ -> (l.(indice ch) <- acc; aux1 sentence' c acc l)
	in aux1 (list sentences) 0 "" (Array.make 27 "");;

let paths tree = 
	let rec aux1 s c acc l = match s with
		|[] -> l
		|ch::s' -> match ch with
			|'(' -> aux1 s' (c + 1) (Bytes.cat acc (Bytes.of_string "0")) l
			|')' -> aux1 s' (c - 1) (Bytes.sub acc 0 c) l
			|'.' -> aux1 s' c (Bytes.cat (Bytes.sub acc 0 (c - 2)) (Bytes.of_string"1") ) l 
			|_ -> (l.(indice ch) <- Bytes.to_string acc; aux1 s' c acc l)
	in aux1 (list tree) 0 (Bytes.of_string "") (Array.make 27 "");;

type bin = Z | A of bin | B of bin;;

let time2 b = A(b);; 

let divid2 b = match b with
	|A(Z) -> b
	|A(b') -> b'
	|B(b') -> b'
	|_ -> Z;;

let plus1 b = match b with
	|A(b') -> B(b')
	| _ -> Z;;

let rec bin_to_string b = match b with
	|Z-> ""
	|A(b') -> (bin_to_string b') ^ "0"
	|B(b') -> (bin_to_string b') ^ "1";;


let paths tree = let l = (Array.make 27 "") in
	let rec aux1 s acc  = match s with
		|[] -> l
		|ch::s' -> match ch with
			|'(' -> aux1 s' (time2 acc)
			|')' -> aux1 s' (divid2 acc)
			|'.' -> aux1 s' (plus1 acc)			
			|_ -> (l.(indice ch) <- (bin_to_string acc) ; aux1 s' acc)
	in aux1 (list tree) Z;;

paths (construit_arbre "elle a mal a la main");;


let compression s = let table = paths (construit_arbre s) in
	let rec aux1 s table = match s with
		|"" -> "Z"
		|_-> table.(indice s.[0]) ^ ( aux1 (String.sub s 1 (String.length s -1)) table)
	in aux1 s table;;


let decompression sc a = 
	let rec aux s t tt = match s, t with
	|"Z", _ -> ""
	|_, Feuille(k) ->  (Char.escaped k) ^ (aux (String.sub s 1 (String.length s -1)) tt tt)
	|_, Noeud(g,d)-> match s.[0] with
		|'0' -> aux (String.sub s 1 (String.length s - 1)) g tt
		|'1' -> aux (String.sub s 1 (String.length s - 1)) d tt
		| _ -> "a"
	in aux sc a a;;







