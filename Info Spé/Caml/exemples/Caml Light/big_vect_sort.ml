
(************************************************************************************************
    Exemple d'utilisation de la bibliothèque big_vect (Jean Mouric , Denis Cazor 03/03/2014)
                (ce fichier s'exécute sans big_vect sur les système 64 bits)
************************************************************************************************)


if sys__word_size = 32 then   (* n'utiliser big_vect que sur les systèmes 32 bits *)
   begin
      directory "exemples/Caml Light/big_vect";
      load "big_vect.ml";
      include "big_vect_open.ml";
   end
;;

let long = 5000000;;
let tab = make_vect long 0;;

(************************************************************************************************
                                        Fonctions de tri
************************************************************************************************)

(* tri fusion *)

let merge_sort tab =
   let l = vect_length tab in
      let tab1 = make_vect l max_int in
         let fusion i j =
            let p = ref i and q = ref (i + j) and k = ref 0 in
               while (!p < i + j) && (!q < i + 2 * j) && (!p < l) && (!q < l) do
                  if tab.(!p) < tab.(!q) then
                     (tab1.(!k) <- tab.(!p);
                        incr p)
                  else (tab1.(!k) <- tab.(!q);
                        incr q);
                  incr k;
               done;
               if !p < i + j then
                  while (!p < i + j) && (!p < l) do
                     tab1.(!k) <- tab.(!p);
                     incr p;
                     incr k;
                  done
               else
                  while (!q < i + 2 * j) && (!q < l) do
                     tab1.(!k) <- tab.(!q);
                     incr q;
                     incr k;
                  done;
               for n = 0 to !k - 1 do
                  tab.(i + n) <- tab1.(n)
               done;
         in
            let a = ref 1 in
               while !a < l do
                  a := !a * 2
               done;
               let j = ref 1 in
                  while !j < !a do
                     let i = ref 0 in
                        while !i + 2 * !j <= !a do
                           fusion !i !j;
                           i := !i + 2 * !j;
                        done;
                        j := 2 * !j;
                  done;;

(* tri rapide *)

let quick_sort data =
   let rec sort (inf, sup) =
      let min = ref inf and max = ref sup
      and pivot = (data.(inf) + data.(sup)) / 2 in
         while !min < !max do
            while data.(!min) < pivot do incr min; done;
            while data.(!max) > pivot do decr max; done;
            if !min <= !max then
               (let temp = data.(!min) in
                     data.(!min) <- data.(!max);
                     data.(!max) <- temp;
                     incr min;
                     decr max);
         done;
         if inf < !max then sort (inf, !max);
         if !min < sup then sort (!min, sup);
   in sort (0, vect_length data - 1);;

(************************************************************************************************
                                        Fonctions de test
************************************************************************************************)

for k = 0 to long - 1 do
   tab.(k) <- random__int (5 * long);
done;
tab;;

let start = sys__time () in
   quick_sort tab;
   print_float (sys__time () -. start);
   print_newline ();
   tab;;

for k = 0 to long - 1 do
   tab.(k) <- random__int (5 * long);
done;
tab;;

let start = sys__time () in
   merge_sort tab;
   print_float (sys__time () -. start);
   print_newline ();
   tab;;

vect_length tab;;

if sys__word_size = 32 then include "big_vect_close.ml";; (* retour aux vecteurs natifs *)


(************************************************************************************************
    Exemple d'utilisation de la bibliothèque big_vect (Jean Mouric , Denis Cazor 03/03/2014)
************************************************************************************************)