include "exemples/Caml Light/Rubik/divers/types.ml";;

type cube1 =
	{
		mutable mouvement1: mouvement1;
		mutable context1: context;
		mutable dessine1: unit -> unit;
		mutable liste_mouvements: bool;
		mutable op_globales1: ops * ops;
		mutable op_externes1: ops * ops * ops * ops;
		mutable op_externes1i: ops * ops * ops * ops;
		mutable op_internes1: ops * ops * ops * ops;
		mutable op_internes1i: ops * ops * ops * ops;
		mutable liste_ops1: string list ref;
		mutable op_from_strings1: string list -> (unit -> unit) list;
		mutable boutons1: bouton vect
	}
;;
