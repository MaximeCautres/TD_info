cd "exemples/Caml Light/inria/minicaml";;
compile "syntaxe.mli";;
compile "eval.mli";;
compile "eval.ml";;
load_object "eval.zo";;
compile "lexuniv.mli";;
compile "lexuniv.ml";;
load_object "lexuniv.zo";;
compile "syntaxe.ml";;
load_object "syntaxe.zo";;
compile "types.mli";;
compile "types.ml";;
load_object "types.zo";;
compile "synthese.mli";;
compile "synthese.ml";;
load_object "synthese.zo";;
compile "caml.ml";;
load_object "caml.zo";;
#open "caml";;

boucle();;

(* Pour ex�cuter une phrase minicaml, la placer ici, comme la phrase suivante:

let f = function x -> x * x in f 3;;

la s�lectionner et l'envoyer � la boucle minicaml.
Noter qu'il est n�cessaire que cette phrase soit ici en commentaire
en raison du ";;"... afin de pouvoir �tre envoy�e � minicaml.
*)