type 'a arbre = Vide | N of 'a arbre * 'a * 'a arbre;;

let rec imprime_infixe arbre = match arbre with
	| Vide -> ()
	| N(g, n, d) -> (imprime_infixe g; print_int n; imprime_infixe d);;

let rec imprime_prefixe arbre = match arbre with
	| Vide -> ()
	| N(g, n, d) -> (print_int n; imprime_prefixe g; imprime_prefixe d);;
	
let rec imprime_postfixe arbre = match arbre with
	| Vide -> ()
	| N(g,n,d) -> (imprime_postfixe g; imprime_postfixe d; print_int n);;

let imprime_largeur arbre = 
	let rec aux l = match l with
		|N(Vide,n,Vide)::l-> (print_int n)
		|N(g,n,Vide)::l-> (print_int n; aux (l@[g]))
		|N(Vide,n,d)::l-> (print_int n; aux (l@[d]))
		|N(g,n,d)::l-> (print_int n; aux (l@[g]@[d]))
		|_ -> ()
		in aux [arbre] ;;

(* Arbre binaire de recherche *)

type abr = V | C of abr * int * abr ;;

let rec test_abr arbre = match arbre with
	|V -> true 
	|C(g,n,d) -> match g, d with
					|V, V -> true
					|V, C(g2,n2,d2) -> n<=n2 && test_abr d
					|C(g1,n1,d1), V -> n>=n1 && test_abr g
					|_,_ -> test_abr(C(g,n,V)) && test_abr(C(V,n,d))
;;

let rec trouve a arbre = match arbre with
	|V -> false
	|C(g,n,d)-> if a = n then true else
					(if a < n then trouve a g 
					else trouve a d);;

let rec insere_feuille a arbre = match arbre with
	|V -> C(V,a,V)
	|C(g,n,d)-> if a < n then C(insere_feuille a g, n, d)
					else C(g, n , insere_feuille a d);;

let rec insere_racine a arbre = match arbre with
	|V;;


let rec supp e arbre = if trouve e arbre then 
	let rec find_r_l arbre = match arbre with
	|C(_,n,V) -> n
	|C(_,_,d)-> find_r_l d in
	let rec reconstruct arbre s = match arbre with
	|V -> V
	|C(g,n,d)-> if n = s
					then C(reconstruct g (find_r_l g) ,find_r_l g, d)
					else C(reconstruct g s, n, reconstruct d s)
	in reconstruct arbre e
	else arbre;;
	
supp 3 (C(C(V,2,V),1,C(V,3,V)));;
	
let rec creer_abr l = match l with
	|[] -> V
	|a::l' -> insere_feuille a (creer_abr l');;

let rec find_r_l arbre = match arbre with
	|C(_,n,V) -> n
	|C(_,_,d)-> find_r_l d;;





(* exercice 3*)

type ope = 
			| E of int 
			| N of ope
			| A of ope*ope
			| M of ope*ope
			| D of ope*ope
			| S of ope*ope;;


let rec eval expre = match expre with
	|E(n) -> n
	|N(op) -> - (eval op)
	|A(op1, op2) -> (eval op1) + (eval op2)
	|M(op1, op2) -> (eval op1) * (eval op2)
	|D(op1, op2) -> (eval op1) / (eval op2)
	|S(op1, op2) -> (eval op1) - (eval op2);;

let rec affiche expre = match expre with
	|E(n) -> string_of_int n
	|N(op) -> "(-"^(affiche op)^")"
	|A(op1, op2) -> "("^(affiche op1)^"+"^(affiche op2)^")"
	|M(op1, op2) -> "("^(affiche op1)^"*"^(affiche op2)^")"
	|D(op1, op2) -> "("^(affiche op1)^"/"^(affiche op2)^")"
	|S(op1, op2) -> "("^(affiche op1)^"-"^(affiche op2)^")";;
	
affiche (A(M(E(3),E(2)),D(E(4),E(2))));;

let rec affiche_p expre = match expre with
	|E(n) -> string_of_int n
	|N(op) -> "_"^(affiche_p op)
	|A(op1, op2) -> "+"^" "^(affiche_p op1)^" "^(affiche_p op2)
	|M(op1, op2) -> "*"^" "^(affiche_p op1)^" "^(affiche_p op2)
	|D(op1, op2) -> "/"^" "^(affiche_p op1)^" "^(affiche_p op2)
	|S(op1, op2) -> "-"^" "^(affiche_p op1)^" "^(affiche_p op2);;

affiche_p (A(M(E(3),E(2)),D(E(4),E(2))));;

(* exercice 4*)

let rank arbre = 
	let rec aux1 arbre = match arbre with
	|Vide -> 0
	|N(g,_,d)-> 1 + (aux1 g) + (aux1 d) 
	in let rec aux arbre = match arbre with
	|Vide -> Vide
	|N(g,n,d)->N(aux g,(n,1+(aux1 g)), aux d)
	in aux arbre;;

rank (N(N(N(Vide,1,Vide),3,N(Vide, 5, Vide)),6,N(Vide,7,N(Vide,9,Vide))));;

let rec trouve a arbre = match arbre with
	|Vide -> false
	|N(g,n,d)-> if a = n then true else
					(if a < n then trouve a g 
					else trouve a d);;

let rank_tot arbre n = match trouve n arbre with
		 |true -> 
		 let rec min arbre = match arbre with
			|Vide -> failwith "no minimum"
			|N(Vide,n,_) -> n
			|N(g,_,_) -> min g
		in let rec aux arbre n = match n, trouve n arbre with
			|n, true when n = min arbre -> 1
			|_, true -> 1 + (aux arbre (n-1))
			|_, false -> aux arbre (n-1)
		in aux arbre n 
		|false -> failwith "l'élement n'est pas dans l'arbre";;

rank_tot (N(N(N(Vide,1,Vide),3,N(Vide, 5, Vide)),6,N(Vide,7,N(Vide,9,Vide)))) 7;;

let rec element_of_rank arbre m = match arbre with
	|Vide -> ()
	|N(g,n,d)-> if rank_tot n = m then n else (element_of_rank g n, element_of_rank d n)

