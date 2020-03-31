let rec list_map l f = match l with
  |[] -> []
  |e :: l' -> (f e) :: (list_map l' f);;

let rec list_select l p = match l with
  |[] -> []
  |e :: l' -> if p e then e :: (list_select l' p) else (list_select l' p);;

let rec insere e l = match l with
  |[] -> [e]
  |n :: l' -> if e < n then e :: l else n :: (insere e l');;

let tri_insertion l =
  let rec aux atrier trier = match atrier with
    |[] ->  trier
    |e :: atrier' -> aux atrier' (insere e trier) in
  aux l [];;


let insertion_dic v = 
        let n = Array.length v in
        for i=1 to (n-1) do
                let p = v.(i) in
                let a = ref 0 in
                let b = ref i in
                while ((!b) - (!a) > 1) do
                        if p > v.(((!b) + (!a))/2) 
                        then a:= ((!b) + (!a))/2
                        else b:= ((!b) + (!a))/2
                                                                                done;
                if p > v.(!a) 
                then
                        (for j=i downto ((!b)+1) do
                                v.(j) <- v.(j-1);    done;
                        v.(!b) <- p;)
                else 
                        (for j=i downto (!b) do
                                v.(j) <- v.(j-1); done;
                        v.(!a) <- p;)
                                                        done;
        v;;
      

insertion_dic [|2; 4; 6; 4; 2; 1|];;
        
        
let rec expo a n = match n with
  |0 -> 1
  |_ -> let k = expo a (n / 2) in match n mod 2 with
        |1 -> a * k * k
        |0 -> k * k 
        |_ -> 0;;

 expo 3 8;;
      
 let rec pgcd a b = let r = a mod b in match r with
                                       |0 -> b
                                       |_ -> pgcd b r;;
                                        
      
 let rec base n x = match x with
   |0 -> []
   |_ -> (base n (x / n)) @ [x mod n];;
 


let list_rev cruche =
        let rec auxiliaire verre = function
                | [] -> verre
                | tete :: queue ->
                        auxiliaire (tete :: verre) queue
        in sub [] cruche

 
 base 2 64 ;;

let rec eval n l = match l with
  |[] -> 0
  |e :: l' -> e + n * (eval n l');;

let addition n x y =
  let aux a b r = match a, b with
    |[], [] -> []
    |z, [] | [], z -> z
    |hx :: tx, hy :: ty -> let s = hx + hy in (s mod n) :: (sub([s/n], (sub (tx, ty))))
  in
  sub(x, y) ;;

