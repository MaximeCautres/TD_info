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

