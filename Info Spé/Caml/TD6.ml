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

let accessible_2 automa = 
	let s = Array.make automa.taille -1 and new_transitions = Array.make automa.taille [] and 
	new_final = Array.make automa.taille false and new_initial = 0 and k = ref 1 in
	let rec aux1 voisins pred = match voisins with
		|[] -> ()
		|(mouv, voisin) :: voisins' -> if s.(voisin) = -1 then 
		(if automa.final.(voisin) then new_final.(k) <- true; s.(voisin) <- !k; 
		new_transitions.(pred) <- (mouv, k)::new_transitions.(pred); 
		k := !k + 1 ; aux1 (automa.transitions.(voisin)) voisin; aux1 voisins' pred)
		else (new_transitions.(pred) <- (mouv, s.(voisin))::new_transitions.(pred)) in
	s.(automa.initial) <- 0;
	aux1 (automa.transitions.(automa.initial)) new_initial;
	let true_taille = k in let true_final = Array.make true_taille false 
	and true_transitions = Array.make true_taille [] in
	for i = 0 to true_taille - 1 do
		true_transitions.(i) <- new_transitions.(i);
		true_final.(i) <- new_final.(i) done;
	let true_automa = {taille = true.taille; initial = new_initial; final = true_final; transitions = true_transitions} in
	true_automa;;
