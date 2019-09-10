cd "exemples/Caml Light/inria/minilogo";;

compile "crayon.mli";;
compile "crayon.ml";;
load_object "crayon.zo";;
compile "langage.mli";;
compile "langage.ml";;
load_object "langage.zo";;
compile "alex.mli";;
compile "alex.ml";;
load_object "alex.zo";;
compile "asynt.mli";;
compile "asynt.ml";;
load_object "asynt.zo";;
compile "logo.ml";;
load_object "logo.zo";;
#open "logo";;

(*
pour carré :c répète 4 [av :c td 90].
      pour multi_carré :c :n répète :n [carré :c td 10].
      ve multi_carré 80 10 .
pour spirale :d :a :i :n
       si :n >= 0 [av :d td :a spirale (:d + :i) :a :i (:n - 1)]
        [stop].
ve spirale
      0 179.5 0.5 360 .
ve spirale
      0 178.5 0.5 360 .
ve spirale
      0 79.8 0.4 360 .
ve spirale
      0 79.5 0.4 360 .
pour spirala :d :a :i :n
  si :n >= 0
    [av :d td :a spirala :d (:a + :i) :i (:n - 1)] [stop].      
ve spirala 10 0 2.5 90 .
ve spirala
      5 0 89.5 1440 .
ve spirala
      4 0.5 181.5 1500 .
*)

boucle()
(* l'instruction "boucle" précédente ayant lancé l'interpréteur
   minilogo, il suffit de cliquer sur le bouton "send" pour
   exécuter le programme minilogo suivant; cliquer sur le bouton
   "interrupt" pour arrêter la boucle *)
;;

pour spirala :d :a :i :n
  si :n >= 0
    [av :d td :a spirala :d (:a + :i) :i (:n - 1)] [stop].

ve spirala
      4 0.5 181.5 1500 .
