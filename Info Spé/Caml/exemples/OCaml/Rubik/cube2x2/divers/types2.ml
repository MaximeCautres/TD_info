type mv2 = {
		mutable rot_coins: int vect;
		mutable perm_coins: int vect};;

type mouvement2 =
	{mutable mv2: mv2}
;;

type cube2 =
	{
		mutable mouvement2: mouvement2;
		mutable context2: context;
		mutable dessine2: unit -> unit;
		mutable op_globales2: ops * ops;
		mutable op_externes2: ops * ops * ops * ops;
		mutable op_internes2: ops * ops * ops * ops;
		mutable liste_ops2: string list ref;
		mutable op_from_strings2: string list -> (unit -> unit) list;
		mutable boutons2: bouton vect
	}
;;
