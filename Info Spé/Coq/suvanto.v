Require Import ZArith.

Inductive color : Set := red | black.

Inductive tr : Set :=
  | L : tr
  | N : tr -> nat -> color -> tr -> tr.

(*----------------------------------------------------------------------*)
(*1. Coder les invariants : fonctions*)
(*----------------------------------------------------------------------*)

(*calcule let ET booleen*)
Definition andb b1 b2 :=
  match b1 with
    | false => false
    | true => b2
end.

(*renvoie si la racine du 1er arbre donnee est plus petite que la racine du 2eme
S'il y a un arbre L, je renvoie true par convention*)
Definition racine_inferieure ab1 ab2 :=
  match ab1, ab2 with
  | N g1 n1 c1 d1, N g2 n2 c2 d2 => n1 <=? n2
  | _, _ => true
end.

Fixpoint inv1 ab :=
  match ab with
  | L => true
  | N g n c d => andb (andb (racine_inferieure ab d) (inv1 d))
                      (andb (racine_inferieure g ab) (inv1 g))
end.

(*renvoie si au moins un des 2 arbres a une racine*)
Definition une_racine_noire ab1 ab2 :=
  match ab1, ab2 with
  | N g1 n1 red d1, N g2 n2 red d2 => false
  | _,_ => true
end.

(*pas de parent rouge pour un noeud rouge revient a dire qu'on ne peut pas avoir
un fils et le parent qui sont rouges, donc le fils ou le parent est noir*)
Fixpoint inv2 ab :=
  match ab with
  | L => true
  | N g n c d => andb (andb (une_racine_noire ab d) (inv2 d))
                      (andb (une_racine_noire g ab) (inv2 g))
end.

(*La profondeur noire de la branche droite*)
Fixpoint une_profondeur_noire ab :=
  match ab with
  | L => 0
  | N g n black d => 1 + une_profondeur_noire d
  | N g n red d => une_profondeur_noire d
end.

(*inv3 meme profondeur noire*)
(*inv3 ab nous assure que LA profondeur noire de abest bien definie
 (et on  peut prendre une profondeur arbitraire)*)
Fixpoint inv3 ab :=
  match ab with
  | L => true
  | N g n c d => andb (andb (inv3 g) (inv3 d))
                      ((une_profondeur_noire g) =? (une_profondeur_noire d))
end.

(*renvoie si la racine est noire (pour simplifier le code de inv4, je renvoie true pour L)*)
Definition racine_est_noire ab :=
  match ab with
  | N g n red d => false
  | _ => true
end. 

Fixpoint inv4 ab :=
  match ab with
  | L => true
  | N g n c d => andb (andb (inv4 g) (inv4 d))
                      (racine_est_noire d)
end.

(*----------------------------------------------------------------------*)
(*2. Coder les invariants : predicat*)
(*----------------------------------------------------------------------*)

(*.
Le fils droit est forcement noir et le parent et le fils gauche ne peuvent pas etre rouges simultanement.
D'ou 3 cas
1. Racine rouge et racine du fils gauche noir
2. Racine noire et racine du fils gauche rouge
3. Racine noire et racine du fils gauche noir

La condition couleur s'ecrit
fils droit noir ET (racine noire OU fils gauche noir)
racine_est_noire d -> (c==black orb racine_est_noire g)

La condition sous arbres rouge et noir penches
rbl d n -> rbl g n

La condition etre arbre binaire de recherche sachant que gauche et droit le sont
racine_inferieure g ab -> racine_inferieure g abab d
*)

Definition est_noir c :=
  match c with
  | black => True
  | red => False
end.

(*On a 1 cas de base et 2 cas de construction ou p differe selon que la racine soit rouge ou non*)
Inductive rbl : tr -> nat -> Prop :=
  | base : rbl L 0
  | constr_black : forall g n d, forall p, 
    rbl d p -> rbl g p
    -> racine_est_noire d = true
    -> racine_inferieure g (N g n black d) = true -> racine_inferieure (N g n black d) d = true
    -> rbl (N g n black d) (1+p)
 | constr_red : forall g n d, forall p, 
    rbl d p -> rbl g p
    -> racine_est_noire d = true -> racine_est_noire g = true
    -> racine_inferieure g (N g n red d) = true -> racine_inferieure (N g n red d) d = true
    -> rbl (N g n red d) p.

(*----------------------------------------------------------------------*)
(*3. Prouver l'equivalence*)
(*----------------------------------------------------------------------*)

Definition etre_arbre_rouge_et_noir_penche_selon_fonction ab :=
inv1 ab = true /\ inv2 ab = true /\ inv3 ab = true /\ inv4 ab = true.

(*Les lemmes i1 i2 i3 i4 sont des lemmes pour prouver les invariants a partir des sous-arbres gauche et droite*)
Lemma i1 : forall g n c d, racine_inferieure (N g n c d) d = true -> inv1 d = true
                        -> racine_inferieure g (N g n c d) = true -> inv1 g = true
                        -> inv1 (N g n c d) = true.
Proof.
intros. simpl. rewrite H0. rewrite H1. rewrite H2. simpl.
unfold racine_inferieure in H. rewrite H. reflexivity.
Qed.

Lemma i2 : forall g n c d, une_racine_noire (N g n c d) d = true -> inv2 d = true
                        -> une_racine_noire g (N g n c d) = true -> inv2 g = true
                        -> inv2 (N g n c d) = true.
Proof.
intros. simpl. rewrite H0. rewrite H1. rewrite H2. simpl.
unfold une_racine_noire in H. rewrite H. reflexivity.
Qed.

Theorem refl_bool : forall x, x =? x = true.
Proof.
induction x. reflexivity. simpl. apply IHx.
Qed.

Lemma i3 : forall g n c d,  inv3 d = true -> (une_profondeur_noire g) = (une_profondeur_noire d)
                         -> inv3 g = true
                         -> inv3 (N g n c d) = true.
Proof.
intros. simpl. rewrite H. rewrite H0. rewrite H1. simpl. apply refl_bool.
Qed.

Lemma i4 : forall g n c d,  inv4 d = true -> racine_est_noire d = true
                         -> inv4 g = true
                         -> inv4 (N g n c d) = true.
Proof.
intros. simpl. rewrite H. rewrite H0. rewrite H1. simpl. reflexivity.
Qed.

(*j'ai mis en lemmes 3 resultats techniques utilisees, dont 2 sur ce qu'implique rbl sur la profondeur noire*)

Lemma rbl_prof : forall p ab, rbl ab p -> une_profondeur_noire(ab) = p.
Proof.
intro p.
intro ab.
  intro H. induction H.
  + trivial.
  + simpl. rewrite IHrbl1. reflexivity.
  + simpl. assumption.
Qed.

Lemma rbl_prof_eq : forall p g d, rbl g p -> rbl d p -> une_profondeur_noire d = une_profondeur_noire g.
Proof.
intros.
assert (une_profondeur_noire d = p).
apply rbl_prof. assumption.
rewrite H1.
assert (une_profondeur_noire g = p).
apply rbl_prof. assumption.
rewrite H2.
reflexivity.
Qed.

Lemma implication_base: 
rbl L 0 -> etre_arbre_rouge_et_noir_penche_selon_fonction L.
Proof.
intro H.
unfold etre_arbre_rouge_et_noir_penche_selon_fonction;
repeat split.
Qed.

Lemma implication_noire:
forall g n d p, 
rbl g p -> rbl d p ->
racine_est_noire d = true ->
etre_arbre_rouge_et_noir_penche_selon_fonction d ->
etre_arbre_rouge_et_noir_penche_selon_fonction g ->
racine_inferieure g (N g n black d) = true ->
racine_inferieure (N g n black d) d = true ->
etre_arbre_rouge_et_noir_penche_selon_fonction (N g n black d).
Proof.
intros g n d p rg rd Hrd Hd Hg Hig Hid.
unfold etre_arbre_rouge_et_noir_penche_selon_fonction.
    unfold etre_arbre_rouge_et_noir_penche_selon_fonction in Hd.
    unfold etre_arbre_rouge_et_noir_penche_selon_fonction in Hg.
    destruct Hd. destruct H0. destruct H1.
    destruct Hg. destruct H4. destruct H5.
    repeat split. 
      apply i1.
        assumption. assumption. assumption. assumption. 

      apply i2.
        reflexivity.
        assumption.
        unfold une_racine_noire.
        destruct g. trivial.
        destruct c. trivial. trivial.
        assumption.

      apply i3.
        assumption.
        apply rbl_prof_eq with p. assumption. assumption.
        assumption. 
        simpl. rewrite Hrd. rewrite H2. rewrite H6. reflexivity.
Qed.

(*Preuve tres similaire a implication_noire au detail pres qu'on a l'hypothese que la racine
de l'arbre gauche est noire*)
Lemma implication_rouge:
forall g n d p, 
rbl g p -> rbl d p ->
racine_est_noire d = true ->
racine_est_noire g = true ->
etre_arbre_rouge_et_noir_penche_selon_fonction d ->
etre_arbre_rouge_et_noir_penche_selon_fonction g ->
racine_inferieure g (N g n red d) = true ->
racine_inferieure (N g n red d) d = true ->
etre_arbre_rouge_et_noir_penche_selon_fonction (N g n red d).
Proof.
intros g n d p rg rd Hrd Hrg Hd Hg Hig Hid.
unfold etre_arbre_rouge_et_noir_penche_selon_fonction.
    unfold etre_arbre_rouge_et_noir_penche_selon_fonction in Hd.
    unfold etre_arbre_rouge_et_noir_penche_selon_fonction in Hg.
    destruct Hd. destruct H0. destruct H1.
    destruct Hg. destruct H4. destruct H5.
    repeat split. 
      apply i1.
        assumption. assumption. assumption. assumption. 

      apply i2.
        assumption. assumption. apply Hrg. 
        (*On utilise l'hypothese Hrg : racine_est_noire g = true
        hypothese dont on n'aurait pas besoin si la racine etait noire*)
        assumption. 

      apply i3.
        assumption.
        apply rbl_prof_eq with p. 
        assumption. assumption. assumption. 
        simpl. rewrite Hrd. rewrite H2. rewrite H6. reflexivity.
Qed.

Theorem implication: 
forall ab, rbl ab (une_profondeur_noire ab) -> etre_arbre_rouge_et_noir_penche_selon_fonction ab.
Proof.
intro ab.
intro H.
(*Inductions sur la structure de rbl ab (une_profondeur_noire ab)*)
induction H.
(*Il y a 3 cas puisqu il y a 3 constructeurs de rbl*)
  (*Case 1 de base*)
  - apply implication_base. apply base.

  (*Cas 2 : racine noire*)
  - apply implication_noire with p.
    (*On utilise toutes les hypothese d'induction*)
    assumption. assumption. assumption. assumption. assumption. assumption. assumption. 
  
  (*Cas 3 : racine rouge*)
  - apply implication_rouge with p.
    (*On utilise toutes les hypothese d'induction*)
    assumption. assumption. assumption. assumption. assumption. assumption. assumption. assumption.
Qed.

Lemma destruct_andb : forall a b, andb a b = true -> a = true /\ b = true.
Proof.
intros a b H. split.
destruct a. destruct b.
trivial. trivial. discriminate.
destruct b. destruct a.
trivial. trivial. destruct a. discriminate. discriminate.
Qed.

(*----------------------------------------------------------------------*)
(*8 lemmes tres similaires exprimant que si N g n c d verifie l'invariant
alors g aussi. Pareil pour d.
+ 1 theoreme qui dit que si un arbre est rouge et noir penche alors ses fils droits et gauches aussi
*)
(*----------------------------------------------------------------------*)

Lemma sous_inv1g : forall g c n d, inv1 (N g c n d) = true -> inv1 g = true.
Proof.
  intros.
  unfold inv1 in H. unfold inv1.
  unfold inv1. 
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H0. destruct H0.
  assumption.
Qed.

Lemma sous_inv1d : forall g c n d, inv1 (N g c n d) = true -> inv1 d = true.
Proof.
  intros.
  unfold inv1 in H. unfold inv1.
  unfold inv1. 
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H0. destruct H0.
  assumption.
Qed.

Lemma sous_inv2g : forall g c n d, inv2 (N g c n d) = true -> inv2 g = true.
Proof.
  intros.
  unfold inv2 in H. unfold inv2.
  unfold inv2. 
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H0. destruct H0.
  assumption.
Qed.

Lemma sous_inv2d : forall g c n d, inv2 (N g c n d) = true -> inv2 d = true.
Proof.
  intros.
  unfold inv2 in H. unfold inv2.
  unfold inv2. 
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H0. destruct H0.
  assumption.
Qed.

Lemma sous_inv3g : forall g c n d, inv3 (N g c n d) = true -> inv3 g = true.
Proof.
  intros.
  unfold inv3 in H. unfold inv3.
  unfold inv3. 
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H. destruct H.
  assumption.
Qed.

Lemma sous_inv3d : forall g c n d, inv3 (N g c n d) = true -> inv3 d = true.
Proof.
  intros.
  unfold inv3 in H. unfold inv3.
  unfold inv3. 
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H. destruct H.
  assumption.
Qed.

Lemma sous_inv4g : forall g c n d, inv4 (N g c n d) = true -> inv4 g = true.
Proof.
  intros.
  unfold inv4 in H. unfold inv4.
  unfold inv4. 
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H. destruct H.
  assumption.
Qed.

Lemma sous_inv4d : forall g c n d, inv4 (N g c n d) = true -> inv4 d = true.
Proof.
  intros.
  unfold inv4 in H. unfold inv4.
  unfold inv4. 
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H. destruct H.
  assumption.
Qed.

Theorem sous_invd : forall g c n d, etre_arbre_rouge_et_noir_penche_selon_fonction (N g c n d) ->
etre_arbre_rouge_et_noir_penche_selon_fonction d.
Proof.
intros.
unfold etre_arbre_rouge_et_noir_penche_selon_fonction.
destruct H. destruct H0. destruct H1.
repeat split.
apply sous_inv1d with g c n. assumption.
apply sous_inv2d with g c n. assumption.
apply sous_inv3d with g c n. assumption.
apply sous_inv4d with g c n. assumption.
Qed.

Theorem sous_invg : forall g c n d, etre_arbre_rouge_et_noir_penche_selon_fonction (N g c n d) ->
etre_arbre_rouge_et_noir_penche_selon_fonction g.
Proof.
intros.
unfold etre_arbre_rouge_et_noir_penche_selon_fonction.
destruct H. destruct H0. destruct H1.
repeat split.
apply sous_inv1g with c n d. assumption.
apply sous_inv2g with c n d. assumption.
apply sous_inv3g with c n d. assumption.
apply sous_inv4g with c n d. assumption.
Qed.

(*----------------------------------------------------------------------*)
(*Fin des petits lemmes*)
(*----------------------------------------------------------------------*)





Lemma prof_eq : forall g n c d, inv3 (N g n c d) = true -> une_profondeur_noire g = une_profondeur_noire d.
Proof.
  intros.
  unfold inv3 in H.
  apply destruct_andb in H. destruct H.
  apply Nat.eqb_eq. assumption.
Qed.

Lemma l1 :forall g n d, inv2 (N g n red d) = true -> racine_est_noire g = true.
Proof.
  intros.
  unfold inv2 in H.
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H. destruct H.
  apply destruct_andb in H0. destruct H0.

  unfold une_racine_noire in H0.
  unfold racine_est_noire. apply H0.
Qed.

Theorem reciproque_rouge: 
forall g n d, etre_arbre_rouge_et_noir_penche_selon_fonction (N g n red d)
 -> (etre_arbre_rouge_et_noir_penche_selon_fonction g -> rbl g (une_profondeur_noire(g)))
 -> (etre_arbre_rouge_et_noir_penche_selon_fonction d -> rbl d (une_profondeur_noire(d)))
 -> rbl (N g n red d) (une_profondeur_noire (N g n red d)).
Proof.

intros g n d. intros.
assert(inv1 (N g n red d) = true).
destruct H. destruct H2. destruct H3. assumption.

assert(inv2 (N g n red d) = true).
destruct H. destruct H3. assumption.

assert(inv3 (N g n red d) = true).
destruct H. destruct H4. destruct H5. assumption.

assert(inv4 (N g n red d) = true).
destruct H. destruct H5. destruct H6. assumption.

simpl.
apply constr_red.
  (*Il y a 5 constructeurs*)
  + simpl. apply H1.
  apply sous_invd with g n red. assumption.
  + 
    assert ((une_profondeur_noire g) = (une_profondeur_noire d)).
    apply prof_eq with n red. destruct H. destruct H2. destruct H3. assumption.
    
    rewrite <- H6.
    apply H0.

    apply sous_invg with n red d. assumption.

  + apply sous_invg in H. unfold inv4 in H5. apply destruct_andb in H5. destruct H5. assumption.

  + apply sous_invg in H.
    apply l1 with n d. 

   unfold inv2 in H3. apply destruct_andb in H2. destruct H2. assumption.

  + unfold inv1 in H2. apply destruct_andb in H2. destruct H2.
    apply destruct_andb in H6. destruct H6. apply H6.

  + unfold inv1 in H2. apply destruct_andb in H2. destruct H2.
    apply destruct_andb in H2. destruct H2. apply H2.
Qed.

(*Meme demo que reciproque_noir au detail pres que les mots red ont ete remplaces par black
et que le constructeur constr_black necessite une hypothese de moins que pour constr_red*)
Theorem reciproque_noir: 
forall g n d, etre_arbre_rouge_et_noir_penche_selon_fonction (N g n black d)
 -> (etre_arbre_rouge_et_noir_penche_selon_fonction g -> rbl g (une_profondeur_noire(g)))
 -> (etre_arbre_rouge_et_noir_penche_selon_fonction d -> rbl d (une_profondeur_noire(d)))
 -> rbl (N g n black d) (une_profondeur_noire (N g n black d)).
Proof.

intros g n d. intros.
simpl.
assert(inv1 (N g n black d) = true).
destruct H. destruct H2. destruct H3. assumption.

assert(inv2 (N g n black d) = true).
destruct H. destruct H3. assumption.

assert(inv3 (N g n black d) = true).
destruct H. destruct H4. destruct H5. assumption.

assert(inv4 (N g n black d) = true).
destruct H. destruct H5. destruct H6. assumption.

simpl.
apply constr_black.
  (*Il y a 4 constructeurs*)
  + simpl. apply H1.
  apply sous_invd with g n black. assumption.
  + 
    assert ((une_profondeur_noire g) = (une_profondeur_noire d)).
    apply prof_eq with n red. destruct H. destruct H2. destruct H3. assumption.
    
    rewrite <- H6.
    apply H0.

    apply sous_invg with n black d. assumption.

  + apply sous_invg in H. unfold inv4 in H5. apply destruct_andb in H5. destruct H5. assumption.

  + unfold inv1 in H2. apply destruct_andb in H2. destruct H2.
    apply destruct_andb in H6. destruct H6. apply H6.

  + unfold inv1 in H2. apply destruct_andb in H2. destruct H2.
    apply destruct_andb in H2. destruct H2. apply H2.
Qed.

Theorem reciproque: 
forall ab, etre_arbre_rouge_et_noir_penche_selon_fonction ab -> rbl ab (une_profondeur_noire ab).
Proof.
intro ab.
intro H.
induction ab.
  + simpl. apply base.
  + destruct c. (*2 case selon la couleur de l'arbre ab*)
    ++ simpl. apply reciproque_rouge. assumption. assumption. assumption. 
    ++ simpl. apply reciproque_noir. assumption. assumption. assumption. 
Qed.


Theorem equivalence : forall ab, rbl ab (une_profondeur_noire ab) <-> etre_arbre_rouge_et_noir_penche_selon_fonction ab.
Proof.
split.
apply implication.
apply reciproque.
Qed.

(*----------------------------------------------------------------------*)
(*4. Inserer un element*)
(*----------------------------------------------------------------------*)


(*La profondeur est toujours bien definie apres l'insertion sans correction
puisqu'on ajoute un noeud rouge !*)

Definition col_noir T :=
match T with
  | L => L
  | N g n c d => N g n black d
end.

(*split et skew sont 2 operations de rotations*)
Definition skew T :=
match T with
  | L => L
  | N g n c L => T
  | N g n c (N gd nd cd dd) => match une_profondeur_noire (N gd nd cd dd) =? une_profondeur_noire T with
    | true => N (N g n c gd) nd cd dd
    | false => T
  end
end.

Definition split T :=
match T with
  | L => L
  | N L n c d => T
  | N (N L ng cd gd) n c d => T
  | N (N gg ng cg dg) n c d => match une_profondeur_noire T =? une_profondeur_noire gg with
    | true => N gg ng cg (N dg n black d)
    | false => T
  end
end.

Fixpoint insert x ab :=
match ab with
  | L => split(skew(N L x red L))
  | N g n c d => match x <? n with
    | true => split(skew(N (insert x g) n c d))
    | false => split(skew(N g n c (insert x d)))
    end
end.

Definition t := N (N (N L 0 black L) 1 red (N L 4 black L)) 5 black (N L 7 black L).

Lemma t_est_rouge_et_noir_penche : rbl t 2.
Proof.
repeat constructor.
Qed.

Definition t_insere x := rbl (insert x t) (une_profondeur_noire (insert x t)).

Lemma test : t_insere 1/\ t_insere 2/\ t_insere 3.
Proof. repeat split. all: repeat constructor. Qed.

