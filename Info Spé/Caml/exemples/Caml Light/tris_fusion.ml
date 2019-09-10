(*
  tri1, tri2 et tri3 fonctionnent avec Caml Light et OCaml, tri4 et tri5 avec Caml Light seulement.
  Sans reconfigurer le collecteur de déchets on obtient en général, du plus efficace au moins efficace,
    avec Caml Light: tri5, tri4, tri1, tri3, tri2
    avec OCaml: tri3, tri2, tri1
*)

let rec tri1 =
  let rec scinde = function
      | t1 :: t2 :: r -> let a, b = scinde r in (t1 :: a, t2 :: b)
      | [t] -> [t], []
      | [] -> [], []
  in
    let rec fusion = function
        | (t1 :: r1 as l1), (t2 :: r2 as l2) -> if t1 <= t2 then t1 :: fusion (r1, l2) else t2 :: fusion (l1, r2)
        | [], l2 -> l2
        | l1, [] -> l1
    in function
        | [] -> []
        | [t] -> [t]
        | l -> let (l1, l2) = scinde l
            in
              fusion ((tri1 l1), (tri1 l2))
;;

let rec tri2 =
  let rec scinde = function
      | t1 :: t2 :: r -> let a, b = scinde r in (t1 :: a, t2 :: b)
      | [t] -> [t], []
      | [] -> [], []
  in
    let rec fusion l1 l2 = match l1, l2 with
        | (t1 :: r1), (t2 :: r2) -> if t1 <= t2 then t1 :: fusion r1 l2 else t2 :: fusion l1 r2
        | [], l2 -> l2
        | l1, [] -> l1
    in function
        | [] -> []
        | [t] -> [t]
        | l -> let (l1, l2) = scinde l
            in
              fusion (tri2 l1) (tri2 l2)
;;

let rec tri3 =
  let scinde l =
    let rec scinde_aux l1 l2 = function
        | t1 :: t2 :: r -> scinde_aux (t1 :: l1) (t2 :: l2) r
        | [t] -> scinde_aux (t :: l1) l2 []
        | [] -> l1, l2
    in
      scinde_aux [] [] l
  in
    let rec fusion l1 l2 = match l1, l2 with
        | (t1 :: r1), (t2 :: r2) -> if t1 <= t2 then t1 :: fusion r1 l2 else t2 :: fusion l1 r2
        | [], l2 -> l2
        | l1, [] -> l1
    in function
        | [] -> []
        | [t] -> [t]
        | l -> let (l1, l2) = scinde l
            in
              fusion (tri3 l1) (tri3 l2)
;;

(* tri4 et tri5 pour Caml Light seulement en raison de fun: multiple matching inconnu d'OCaml *)

let rec tri4 =
  let rec scinde = function
      | t1 :: t2 :: r -> let a, b = scinde r in (t1 :: a, t2 :: b)
      | [t] -> [t], []
      | [] -> [], []
  in
    let rec fusion = fun
        | (t1 :: r1 as l1) (t2 :: r2 as l2) -> if t1 <= t2 then t1 :: fusion r1 l2 else t2 :: fusion l1 r2
        | [] l2 -> l2
        | l1 [] -> l1
    in function
        | [] -> []
        | [t] -> [t]
        | l -> let (l1, l2) = scinde l
            in
              fusion (tri4 l1) (tri4 l2)
;;

let rec tri5 =
  let scinde l =
    let rec scinde_aux l1 l2 = function
        | t1 :: t2 :: r -> scinde_aux (t1 :: l1) (t2 :: l2) r
        | [t] -> scinde_aux (t :: l1) l2 []
        | [] -> l1, l2
    in
      scinde_aux [] [] l
  in
    let rec fusion = fun
        | (t1 :: r1 as l1) (t2 :: r2 as l2) -> if t1 <= t2 then t1 :: fusion r1 l2 else t2 :: fusion l1 r2
        | [] l2 -> l2
        | l1 [] -> l1
    in function
        | [] -> []
        | [t] -> [t]
        | l -> let (l1, l2) = scinde l
            in
              fusion (tri5 l1) (tri5 l2)
;;


let tris_fusion taille =
  let tris = [|tri5; tri4; tri3; tri2; tri1|] and data = ref [] in
    for i = 0 to taille - 1 do
      data := (random__int (5 * taille)) :: !data;
    done;
    for i = 0 to 4 do
      let t = sys__time () and _ = tris.(i) !data in
        print_string ("tri" ^ string_of_int (5 - i) ^ " d'une liste d'entiers de taille " ^ string_of_int taille ^ " en :  ");
        print_float (sys__time () -. t);
        print_string " s";
        print_newline ();
    done;
;;

let taille = 5000000;;

print_string "sans reconfiguration du collecteur de déchets: patience!\n";;
tris_fusion taille;;

(* ----------------------------------------------------------------------------------------------- *)
(* reconfiguration du collecteur de déchets *)

#open "gc";;
let r = gc__get () in
  r.minor_heap_size <- 1 lsl 25;
  gc__set r
;;

(* ----------------------------------------------------------------------------------------------- *)

print_string "après reconfiguration du collecteur de déchets...\n";;
tris_fusion taille;;
