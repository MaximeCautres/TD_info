type mouvement1 =
	{mutable mv1: (int vect * int vect vect) list}
;;

type context =
	{mutable matrice: int vect vect}
;;

type ops = OPS of (unit -> unit) * (unit -> unit) * (unit -> unit);;

type bouton = {titre: string;
		orx: int; ory: int;
		largeur: int; hauteur: int;
		mutable couleur: int;
		mutable action: unit -> unit};;

(*- boutons de saisie d'une configuration du cube à partir des couleurs des faces visibles des minicubes -*)
(* chaque face visible de minicube devient un bouton de type 'btn' *)
(* ces boutons ne permettent ni de distinguer des minicubes centraux de même couleur du cube 4x4 *)
(* ni de figurer la rotation des cubes centraux des faces du cube 3x3 *)
(* aucune vérification n'est faite en cours de saisie *)
type btn =
	{
		mutable btn_centre: int vect;
		mutable btn_faces: int vect vect vect;
		mutable btn_couleurs: string vect
	}
;;
