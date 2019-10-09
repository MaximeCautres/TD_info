type 'a arbre = Nil | N of 'a * 'a arbre * 'a arbre;;
type 'a tas = {tab : 'a array ; mutable taille : int};;
let nmax = 1000;;

let creerTasVide valeur = {taille = 0; tab = Array.make nmax valeur};;

let echange tas i j = let temp = tas.tab.(i) in
								tas.tab.(i) <- tas.tab.(j);
								tas.tab.(j) <- temp;;

let rec remonte tas i = match i with
	|1 -> ()
	|_ -> if tas.tab.(i)>tas.tab.(i/2) 
			then (echange tas i (i/2); remonte tas (i/2));;

let rec descente tas i =
	if (tas.tab.(i) < tas.tab.(2*i)) && (2*i <= tas.taille )
	then (echange tas i (2*i); descente tas (2*i))
	else (if  tas.tab.(i) < tas.tab.(2*i+1) && (2*i+1 <= tas.taille)
 			then echange tas i (2*i+1); descente tas (2*i+1));;

let ajoute element tas =  tas.taille <- tas.taille + 1; 
									tas.tab.(tas.taille) <- element; 
									remonte tas tas.taille;;


let creerTas1 t = let tas = creerTasVide 0 in
			for i = 0 to (Array.length t - 1) do
				ajoute t.(i) tas
			done; tas;;

creerTas1 [|3;1;2;5;4|];;

let creerTas2 t = let tas = creerTasVide 0 in
		for i = 1 to (Array.length t) do
			tas.tab.(i) <- t.(i-1);
		done;
		tas.taille <- (Array.length t);
		for i = 1 to tas.taille do 
			descente tas i
		done; tas;;

(creerTas2 [|3;1;2;5;4|]).taille ;;

let extraitMax tas = let c = tas.tab.(1) in
							tas.tab.(1) <- tas.tab.(tas.taille);
							tas.taille <- tas.taille - 1;
							descente tas 1;
							c;;

extraitMax (creerTas2 [|3;1;2;5;4|] );;



