type expr_rat = 
	Vide 
	| Epsilon 
	| Lettre of char
	| Concat of expr_rat * expr_rat
	| Plus of expr_rat * expr_rat
	| Etoile of expr_rat;;


type expr_lin = 
	Empty
	| Epsi
	| Pos of int
	| Prod of expr_lin * expr_lin
	| Sum of expr_lin * expr_lin
	| Star of expr_lin;;

type 'a arbre = Void | Cons of 'a arbre * 'a * 'a arbre;;

let e = Concat (Concat (Etoile (Plus (Lettre 'a', Concat (Lettre 'a', Lettre 'b'))), Lettre 'a'), Lettre 'b');; 

let linearise expr = 
	let k = ref 0 in
	let rec nb_var expr = match expr with
		|Vide -> ()
		|Epsilon -> ()
		|Lettre (lettre) -> (k := !k + 1)
		|Concat (expr1, expr2) -> (nb_var expr1; nb_var expr2)
		|Plus (expr1, expr2) -> (nb_var expr1; nb_var expr2)
		|Etoile (expr') -> nb_var expr' in
	nb_var expr;
	let table = Array.make !k 'a' in
	let rec auxlin expr= match expr with
		|Vide -> Empty
		|Epsilon -> Epsi
		|Lettre lettre -> (table.(!k - 1) <- lettre; k := !k - 1; Pos (!k + 1))
		|Concat (expr1, expr2) -> Prod (auxlin expr1, auxlin expr2)
		|Plus (expr1, expr2) -> Sum (auxlin expr1, auxlin expr2)
		|Etoile expr' -> Star (auxlin expr') in
	auxlin expr, table ;;

let e', table = linearise e;;

let rec accepteMotVide expr = match expr with
	|Empty -> true
	|Epsi -> true
	|Pos k -> false
	|Prod (expr1, expr2) -> accepteMotVide expr1 && accepteMotVide expr2
	|Sum (expr1, expr2) -> accepteMotVide expr1 || accepteMotVide expr2
	|Star expr' -> true;;

accepteMotVide e';;

let rec prefixes expr = match expr with
	|Empty -> []
	|Epsi -> []
	|Pos k -> [k]
	|Prod (expr1, expr2) -> if accepteMotVide expr1 then prefixes expr1 @ prefixes expr2 
																  else prefixes expr1
	|Sum (expr1, expr2) -> prefixes expr1 @ prefixes expr2
	|Star expr' -> prefixes expr';;

prefixes e';;

let rec suffixes expr = match expr with
	|Empty -> []
	|Epsi -> []
	|Pos k -> [k]
	|Prod (expr1, expr2) -> if accepteMotVide expr2 then suffixes expr1 @ suffixes expr2 
																  else suffixes expr2
	|Sum (expr1, expr2) -> suffixes expr1 @ suffixes expr2
	|Star expr' -> suffixes expr';;

suffixes e';;

let rec inside l i = match l with 
	|[] -> false
	|e :: l' -> if e = i then true else inside l' i;; 

let rec suivant expr i = match expr with
	|Empty -> []
	|Epsi -> []
	|Pos k -> []
	|Prod (expr1, expr2) -> if inside (suffixes expr1) i then match expr1 with 
																				|Star _ -> prefixes expr1 @ prefixes expr2
																				|_ -> prefixes expr2
																		else (suivant expr1 i @ suivant expr2 i)
	|Sum (expr1, expr2) -> (suivant expr1 i) @ (suivant expr2 i)
	|Star expr' -> if inside (suffixes expr') i then prefixes expr' else suivant expr' i ;;

suivant e' 1;;

type automate = {nb_etat : int; finaux : int list; transitions : ((char * int) list) array};;

let glushkov rat = 
	let lin, table = linearise rat in
	let nb_etat = Array.length table + 1 and finaux = suffixes lin in
	let transi = Array.make nb_etat ('a', 1) in
	for i  = 0 to 
	q














