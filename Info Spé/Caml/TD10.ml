let a = 2 ;;

type sommet = int;;
type poids = P of int | Infini ;;
type liste_adj = (sommet * int) list array;;
type matrice_pond = poids array array;;

(* Deux graphes d'exemple *)

let ex01 = 
[|[(1,1);(2,3)];[(0,1);(2,1);(3,2)];[(0,3);(1,1);(4,4)];
  [(1,2);(4,2);(5,6)];[(2,4);(3,2);(5,2)];[(3,6);(4,2)]|]

let ex02 = 
[|[(1,6);(2,9)];[(0,6);(2,5);(3,8);(6,6)];[(0,9);(1,5);(3,4);(4,8);(5,7)];
  [(1,8);(2,4);(5,4);(6,5)];[(2,8);(5,9);(7,4)];[(2,7);(3,4);(4,9);(6,3);(7,10)];
  [(1,6);(3,5);(5,3);(7,6)];[(4,4);(5,10);(6,6)]|];;

type chemin = int list;;

let adj2pond list =
  let n = Array.length list in
  let matrix = Array.make_matrix n n Infini in
  let rec aux1 l i = match l with
    |[] -> ()
    |(j, v) :: l' -> (matrix.(i).(j) <- (P v); aux1 l' i)
  in
  for i = 0 to (n - 1) do
    aux1 list.(i) i
  done;
  matrix;;
    
adj2pond ex01;;


let pond2adj matrix =
  let n = Array.length matrix in
  let liste = Array.make n [] in
  for i = 0 to n - 1 do
    let l = ref [] in
    for j = 0 to n - 1 do
      match matrix.(i).(n-1-j) with
      |Infini -> ()
      |P(v) -> l := (n - 1 - j, v):: !l

    done ;
    liste.(i) <- !l
  done ;
  liste;;
      
          
        
pond2adj (adj2pond ex01);;


let somme a b = match (a, b) with
  |Infini, _ -> Infini
  |_, Infini -> Infini
  |P x, P y -> P (x+y);;

somme (P 3) (P 6) ;;

let inf_s a b = match (a, b) with
  |Infini, _ -> false
  |_, Infini -> true
  |P(a), P(b) -> a < b;;

let copie_matrice matrice =
  let n = Array.length matrice in
  let n_matrice = Array.make_matrix n n Infini in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      n_matrice.(i).(j) <- matrice.(i).(j)
    done;
  done;
  n_matrice;;

let floyd_warshall proper_matrice =
  let d = copie_matrice proper_matrice in
  let n = Array.length d in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      for k = 0 to n - 1 do
        if inf_s (somme d.(i).(k) d.(k).(j)) d.(i).(j) then
        d.(i).(j) <- somme d.(i).(k) d.(k).(j)
      done;
    done;
  done;
  d;;

floyd_warshall (adj2pond ex01);;
adj2pond ex01;;

type couleur = Blanc | Noir;;

let rec mini l = match l with
  |[] -> failwith "Gros naze"
  |e::[] -> e
  |(v, P p) :: (v', P p') :: l' -> if p < p' then mini ((v,P p) :: l') else mini ((v',P p') :: l');;

let enleve_mini l =
  let (v, m) = mini l in
  let rec aux l = match l with
    |[] -> []
    |(a, b) :: l' -> if b = m && v = a then l' else (a,v)::(aux l')
  in
  aux l;;
  
let rec est_dans s l =match l with
  |[] -> false
  |(a, b) :: l' -> if a = s then true else est_dans s l';;

let rec len l = match l with
  |[] -> 0
  |e::l' -> 1 + (len l');;

let dijsktra liste_adj s =
  let f = ref [(s, P 0)] in
  let n = Array.length liste_adj in
  let state = Array.make n Blanc in
  while len !f <> 0 do
    (u, _) = mini !f;
    f := enleve_mini !f;
    state.(u) <- Noir
    for j = 0 to Array.length liste_adj.(u) 
          
  done;
  
                
