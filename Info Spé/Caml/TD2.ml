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








