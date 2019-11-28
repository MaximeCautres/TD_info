type  automate = {
taille : int;
initial : int;
final : bool array;
transitions : (char * int) list array;
};;

let a = {taille = 1; initial = 0; final = [|true|]; transitions = [|[('a', 0)]|]};;
let b = {taille = 1; initial = 0; final = [|true|]; transitions = [|[('b', 0)]|]};;

let mot_vide = {taille = 1; initial = 0; final = [|true|]; transitions = [|[]|]};;
let language_vide = {taille = 1; initial = 0; final = [|false|]; transitions = [|[]|]};;

let rec check_in carac l = match l with
	|[] -> (6, false)
	|(letter, next) :: l' -> if letter = carac then (next, true)  else check_in carac l';;

let est_accepte str auto = 
	let state = ref auto.initial and over_state = ref true in
	for i = 0 to (String.length str - 1) do
		let next_state, possibility = check_in str.[i] auto.transitions.(!state) in
		if possibility then state := next_state else over_state := false 
	done;
	auto.final.(!state) && !over_state ;;

est_accepte "aaaaaaa" b;;

let test = {taille = 5; initial = 0; final = [|false; true; false; true; false|]; transitions = [|[('a', 1); ('b', 2)]; [('a', 1)]; [('a', 3); ('b', 4)]; [('b', 4)]; [('a', 3)]|]};;

let test2 = {taille = 6; initial = 0; final = [|false; true; false; true; false; false|]; transitions = [|[('a', 1); ('b', 2)]; [('a', 1)]; [('a', 3); ('b', 4)]; [('b', 4)]; [('a', 3)]; []|]};;


est_accepte "aa" test;;
est_accepte "aba" test;;
est_accepte "bababababababa" test;;

let accessible automa = 
	let s = Array.make automa.taille true in
	let rec aux1 voisins = match voisins with
		|[] -> ()
		|(_, voisin) :: voisins' -> if s.(voisin) then 
		(s.(voisin) <- false ; aux1 (automa.transitions.(voisin)); aux1 voisins') in
	s.(automa.initial) <- false;
	aux1 (automa.transitions.(automa.initial));
	s;;

accessible test2;;


