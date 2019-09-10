Module NatList.
Set Warnings "-notation-overridden,-parsing".

Fixpoint fact n:nat := match n with
  | 0 => 1
  | S p => n * fact p
  end.
Eval compute in fact 6.


Definition carre n:nat := n * n.

Definition compo (f g:nat->nat) := fun x : nat => g(f x).

Definition f0 := compo S carre.
Eval compute in f0 5.


Inductive day : Type :=
  |monday
  |tuesday
  |wednesday
  |thursday
  |friday
  |saturday
  |sunday.

Definition next_weekday (d:day) :day := match d with
  |monday => tuesday
  |tuesday => wednesday
  |wednesday => thursday
  |thursday => friday
  |_ => monday
  end.

Eval compute in next_weekday friday.
Compute (next_weekday (next_weekday sunday)).

Example test_next_weekday:
  (next_weekday(next_weekday saturday)) = tuesday.

Proof. simpl. reflexivity. Qed.

Inductive bool : Type :=
  |true
  |false.

Definition negb (b:bool):bool:=match b with
  | true => false
  | false => true
  end.

Definition andb (b1:bool) (b2:bool):bool := match b1 with
  |true => b2
  |false => false
  end.

Definition orb (b1:bool) (b2:bool):bool := match b1 with
  |true => true
  |false => b2
  end.

Example test_orb1: (orb false false) = false.
Proof. simpl. reflexivity. Qed.
Example test_orb2: (orb false true) = true.
Proof. simpl. reflexivity. Qed.
Example test_orb3: (orb false true) = true.
Proof. simpl. reflexivity. Qed.
Example test_orb4: (orb true true) = true.
Proof. simpl. reflexivity. Qed.

Notation "x && y" := (andb x y).
Notation "x || y" := (orb x y).
Example test: (true || false || false) = true.
Proof. simpl. reflexivity. Qed.

Definition nandb (b1:bool) (b2:bool):bool:= negb (andb b1 b2).

Example test_nand1: (nandb false false) = true.
Proof. simpl. reflexivity. Qed.
Example test_nand2: (nandb false true) = true.
Proof. simpl. reflexivity. Qed.
Example test_nand3: (nandb false true) = true.
Proof. simpl. reflexivity. Qed.
Example test_nand4: (nandb true true) = false.
Proof. simpl. reflexivity. Qed.

Definition andb3 (b1:bool) (b2:bool) (b3:bool):bool:=andb b1 (andb b2 b3).

Example test_and31: (andb3 true true true) = true.
Proof. simpl. reflexivity. Qed.
Example test_and32: (andb3 false true true) = false.
Proof. simpl. reflexivity. Qed.
Example test_and33: (andb3 false true false) = false.
Proof. simpl. reflexivity. Qed.
Example test_and34: (andb3 true true false) = false.
Proof. simpl. reflexivity. Qed.

Check andb3.

Inductive rgb : Type :=
  |red 
  |blue
  |green.

Inductive color : Type :=
  |black
  |white
  |primary (p : rgb).

Definition monochrome (c : color) : bool := match c with
  | black => true
  | white => true
  | primary c => false
  end.

Definition isred (c:color): bool := match c with
  |black=>false
  |white=>false
  |primary red => true 
  |primary _ => false
  end.

Inductive bit : Type :=
  |B0
  |B1.

Inductive nibble : Type :=
  | bits (b0 b1 b2 b3 : bit).

Definition all_zero (nb:nibble) : bool := match nb with
  |(bits B0 B0 B0 B0) => true
  |(bits _ _ _ _ ) => false
  end.

Compute (all_zero (bits B1 B0 B1 B0)).
Compute (all_zero (bits B0 B0 B0 B0)).

Module NATPLAYGROUND.

Inductive nat : Type :=
  |O
  |S (n : nat).

Check (S (S O)).

Definition pred (n:nat):nat := match n with
  |O=>O
  |S p => p
  end.

Compute (pred (S (S O))).

End NATPLAYGROUND.

Check (S (S (S (S 0)))).

Definition minustwo (n:nat):nat := match n with
  |O=>O
  |S O=> O
  |S (S n)=>n
  end.

Compute (minustwo 7).

Check S.
Check pred.
Check minustwo.

Fixpoint evenb (n:nat):bool := match n with
  |O => true
  |S O => false
  |S (S p)=> evenb p
  end.


Definition oddb (n:nat):bool := negb (evenb n).

Example even1 : (evenb 4) = true.
Proof. simpl. reflexivity. Qed.

Module NATPLAYGROUND2.

Fixpoint plus (n m :nat) : nat := match n with 
  |O => m
  |S p => plus p (S m)
  end.

Compute (plus 3 7).

Notation "x + y" := (plus x y).

Fixpoint mult (n m :nat) :nat := match n with
  |O=>O
  |S p => plus m (mult p m)
  end.

Example mult1 : (mult 3 4) = 12.
Proof. simpl. reflexivity. Qed.

Fixpoint minus (n m :nat) :nat := match n, m with
  |0 , _ => 0
  |_ , 0 => n
  |S p, S pp => minus p pp
  end.

End NATPLAYGROUND2.

Fixpoint exp (base power :nat) :nat := match power with
  |O => S O
  |S p => mult base (exp base p)
  end.

Fixpoint facto (n:nat):nat := match n with
  |O => S O
  |S p => mult n (facto p)
  end.

Example test_factorial2: (facto 5) = (mult 10 12).
Proof. simpl. reflexivity. Qed.

Notation "x + y" := (plus x y)(at level 50, left associativity) :nat_scope.
Notation "x - y" := (minus x y)(at level 50, left associativity) :nat_scope.
Notation "x * y" := (mult x y)(at level 40, left associativity) :nat_scope.
Check ((0 + 1) + 1).
Check ((2+3)*4).

Fixpoint eqb (n m :nat) :bool := match n, m with
  |O, O => true
  |0, _ => false
  |_, 0 => false
  |S n', S m' => eqb n' m'
  end.

Fixpoint eq (n m :nat) :bool := match n with
  |O => match m with 
    |0=>true
    |S m'=> false
    end
  |S n' => match m with
    |0=> false
    |S m'=>eq n' m'
    end
  end.

Fixpoint leb(n m :nat) :bool := match n with
  |O => true
  |S n'=> match m with
    |O => false
    |S m'=> leb n' m'
    end
  end.

Notation " x =? y " := (eqb x y)(at level 70):nat_scope.
Notation " x <=? y" := (leb x y)(at level 70):nat_scope.

Example leb1: ( 2 + 3 <=? 2 * 6) = true.
Proof. simpl. reflexivity. Qed.

Fixpoint ltb (n m :nat) :bool := match n, m with
  |_,0=> false
  |0,_=> true
  |S n', S m'=> ltb n' m'
  end.

Notation "x <? y" := (ltb x y)(at level 70):nat_scope.
Example ltb1: ( 2 + 4 <? 2 * 4) = true.
Proof. simpl. reflexivity. Qed.

Theorem plus_O_n : forall n :nat, 0 + n = n.
Proof. intros n. reflexivity. Qed.

Theorem plus_1_n : forall n :nat, 1 + n = S n.
Proof. intros n. reflexivity. Qed.

Theorem mult_0 : forall n :nat, O * n = O.
Proof. intros n. reflexivity. Qed.

Theorem plus_id_example : forall n m :nat , n = m -> n + n= m + m.
Proof. 
intros n m.
intros H.
rewrite <- H.
reflexivity. Qed.

Theorem plus_id_exercise : forall n m o : nat,
  n = m -> m = o -> n + m = m + o.
Proof. intros n m o. 
intros H1 H2. 
rewrite -> H1. 
rewrite -> H2. 
reflexivity. Qed.

Theorem mult_0_plus : forall n m : nat,
  (0 + n) * m = n * m.
Proof.
  intros n m.
  rewrite -> plus_O_n.
reflexivity. Qed.

Theorem mult_S_1 : forall n m : nat,
  m = S n ->
  m * (1 + n) = m * m.

Proof.
  intros n m.
  intros H.
  rewrite -> plus_1_n.
  rewrite <- H.
reflexivity. Qed.

Theorem plus_1_neq_0 : forall n : nat,
  (n + 1) =? 0 = false.
Proof.
  intros n.
  destruct n as [|n'] eqn:E.
  - reflexivity.
  - reflexivity. Qed.

Theorem negb_involutive : forall b : bool,
  negb (negb b) = b.

Proof.
  intros b. destruct b eqn:E.
  -reflexivity.
  -reflexivity. Qed.

Theorem andb_commutative: forall b c, andb b c = andb c b.
Proof. intros b c. destruct b eqn:E.
  - destruct c eqn:F.
    + reflexivity.
    + reflexivity.
  - destruct c eqn:G.
    + reflexivity.
    + reflexivity. Qed.

Theorem andb_commutative' : forall b c, andb b c = andb c b.
Proof.
  intros b c. destruct b eqn:Eb.
  { destruct c eqn:Ec.
    { reflexivity. }
    { reflexivity. } }
  { destruct c eqn:Ec.
    { reflexivity. }
    { reflexivity. } }
Qed.

Theorem andb3_exchange :
  forall b c d, andb (andb b c) d = andb (andb b d) c.
Proof. intros b c d. destruct b eqn:Eb.
  - destruct c eqn:Ec.
    + destruct d eqn:Ed.
      * reflexivity.
      * reflexivity.
    + destruct d eqn:Ed.
      * reflexivity.
      * reflexivity.
  - destruct c eqn:Ec.
    + destruct d eqn:Ed.
      * reflexivity.
      * reflexivity.
    + destruct d eqn:Ed.
      * reflexivity.
      * reflexivity. Qed.

Theorem plus_1_neq_0' : forall n : nat,
  (n + 1) =? 0 = false.
Proof.
  intros [|n'].
  - reflexivity.
  - reflexivity. Qed.

Theorem andb_commutative'' :
  forall b c, andb b c = andb c b.
Proof.
  intros [] [].
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - reflexivity.
Qed.

Theorem andb_true_elim2 : forall b c : bool,
  andb b c = true -> c = true.
Proof.
  intros b c. intros H. destruct b eqn:Eb.
  - destruct c eqn:Ec.
    + rewrite <- H. reflexivity.
    + rewrite <- H. reflexivity.
  - destruct c eqn:Ec.
    + rewrite <- H. reflexivity.
    + rewrite <- H. reflexivity.
Qed.

Theorem zero_nbeq_plus_1 : forall n : nat,
  0 =? (n + 1) = false.
Proof.
  intros [|n'].
  - reflexivity.
  - reflexivity. Qed.

Theorem identity_fn_applied_twice :
  forall (f : bool -> bool),
  (forall (x : bool), f x = x) ->
  forall (b : bool), f (f b) = b.
Proof.
  intros f.
  intros H.
  intros [].
  - rewrite -> H. rewrite -> H. reflexivity.
  - rewrite -> H. rewrite -> H. reflexivity. Qed.

Theorem negb_fn_applied_twice : 
  forall (f : bool -> bool),
  (forall (x :bool), f x = negb x) ->
  forall (b : bool), f (f b) = b.

Proof.
  intros f.
  intros H. 
  intros [].
  - rewrite -> H. rewrite -> H. reflexivity.
  - rewrite -> H. rewrite -> H. reflexivity.
Qed.

From Coq Require Export String.
Definition manual_grade_for_negation_fn_applied_twice : option (nat*string) := None.

Theorem andb_eq_orb :
  forall (b c : bool),
  (andb b c = orb b c) ->
  b = c.
Proof.
  intros [] []. 
  - reflexivity.
  - simpl. intros H. rewrite -> H. reflexivity.
  - simpl. intros H. rewrite -> H. reflexivity.
  - intros H. reflexivity. Qed.

Inductive bin : Type :=
  | Z
  | A (n : bin)
  | B (n : bin).

Fixpoint incr (m:bin) : bin :=  match m with
  | Z => B Z
  | A m' => B m'
  | B m' => A (incr m')
  end.

Fixpoint bin_to_nat (m:bin) : nat := match m with
  | Z => 0
  | A m' => 2 * (bin_to_nat m')
  | B m' => 1 + 2 * (bin_to_nat m')
  end.

Example test_bin_incr1: (bin_to_nat (incr (B (B Z)))) = (bin_to_nat (B (B Z)) + 1).
Proof. reflexivity. Qed.
Example test_bin_incr2: (bin_to_nat (incr (A (A (B Z))))) = (bin_to_nat (A (A (B Z))) + 1).
Proof. reflexivity. Qed.
Example test_bin_incr3: (bin_to_nat (incr (A (B (B Z))))) = (bin_to_nat (A (B (B Z))) + 1).
Proof. reflexivity. Qed.
Example test_bin_incr4: (bin_to_nat (incr (Z))) = (bin_to_nat (Z) + 1).
Proof. reflexivity. Qed.
Example test_bin_incr5: (bin_to_nat (incr (A (A (A (B Z)))))) = (bin_to_nat (A (A (A (B Z)))) + 1).
Proof. reflexivity. Qed.

Definition manual_grade_for_binary : option (nat*string) := None.

(* beginning of the third chapiter *)

Theorem plus_n_O : forall n:nat, n = n + 0.
Proof.
  intros n. induction n as [|n' IHn'].
  - reflexivity.
  - simpl. rewrite <- IHn'. reflexivity. Qed.

Theorem minus_diag : forall n,
  minus n n = 0.

Proof.
  intros n. induction n as [|n' IHn'].
  - reflexivity.
  - simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem mult_0_r : forall n:nat,
  n * 0 = 0.
Proof.
  intros n. induction n as [|n' IHn'].
  - reflexivity.
  - simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem plus_n_Sm : forall n m : nat,
  S (n + m) = n + (S m).
Proof.
  intros n m. induction n as [|n' IHn'].
  - simpl. reflexivity. 
  - simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem plus_comm : forall n m : nat,
  n + m = m + n.
Proof.
  intros n m. induction n as [|n' IHn'].
  - simpl. rewrite <- plus_n_O. reflexivity.
  - simpl. rewrite <- plus_n_Sm. 
    rewrite <- IHn'. reflexivity. Qed.

Theorem plus_assoc : forall n m p : nat,
  n + (m + p) = (n + m) + p.
Proof.
  intros n m p. induction n as [|n' IHn'].
  -simpl. reflexivity.
  -simpl. rewrite -> IHn'. reflexivity. Qed.

Fixpoint double (n:nat) :=
  match n with
  | O => O
  | S n' => S (S (double n'))
  end.

Lemma double_plus : forall n, double n = n + n .
Proof.
  intros n. induction n as [|n' IHn'].
  -reflexivity.
  -simpl. rewrite <- plus_n_Sm. 
    rewrite <- IHn'. reflexivity. Qed.

Theorem evenb_S : forall n : nat,
  evenb (S n) = negb (evenb n).
Proof.
  intros n. induction n as [|n' IHn'].
  - simpl. reflexivity.
  - assert (H: evenb (S (S n')) = negb (evenb (S n'))).
  { rewrite -> IHn'. rewrite -> negb_involutive. reflexivity. }
  rewrite -> H. reflexivity. Qed.



Definition manual_grade_for_destruct_induction : option (nat*string) := None.

Theorem mult_0_plus' : forall n m : nat,
  (0 + n) * m = n * m.
Proof.
  intros n m.
  assert (H: 0 + n = n). { reflexivity. }
  rewrite -> H.
  reflexivity.  Qed.

Theorem plus_rearrange : forall n m p q : nat,
  (n + m) + (p + q) = (m + n) + (p + q).
Proof. 
  intros n m p q.
  assert (H : n + m = m + n ). { rewrite -> plus_comm. reflexivity. }
  rewrite -> H.
  reflexivity.
Qed.

Definition manual_grade_for_plus_comm_informal : option (nat*string) := None.

Theorem plus_swap : forall n m p : nat,
  n + (m + p) = m + (n + p).
Proof.
  intros n m p.
  rewrite -> plus_assoc.
  rewrite -> plus_assoc.
  assert (H: n + m = m + n).
  { rewrite -> plus_comm. reflexivity. }
  rewrite -> H.
  reflexivity.
Qed.

Theorem mult_S_r: forall m n : nat, m * (S n) = m + m * n.
Proof.
  intros m n. induction m as[|m' IHm']. 
  -simpl. reflexivity.
  -simpl. rewrite -> IHm'. rewrite -> plus_assoc. 
  rewrite -> plus_assoc. 
  assert (H: n + m' = m' + n).
  { rewrite -> plus_comm. reflexivity. }
  rewrite -> H. reflexivity. Qed.


Theorem mult_comm : forall m n : nat,
  m * n = n * m.
Proof. 
  intros m n.
  induction n as [|n' IHn'].
  - simpl. rewrite -> mult_0_r. reflexivity.
  - simpl. rewrite -> mult_S_r. rewrite <- IHn'. reflexivity. Qed.
       
Check leb.

Theorem leb_refl : forall n:nat,
  true = (n <=? n).
Proof.
  intros n. induction n as [|n' IHn'].
  -simpl. reflexivity.
  -simpl. rewrite <- IHn'. reflexivity. Qed.

Theorem zero_nbeq_S : forall n:nat,
  0 =? (S n) = false.
Proof.
  intros n. destruct n as [|n']eqn:E.
  - simpl. reflexivity.
  - simpl. reflexivity. Qed.

Theorem andb_false_r : forall b : bool,
  andb b false = false.
Proof.
  intros b. destruct b as [] eqn:E.
  -reflexivity.
  -reflexivity. Qed.
 
Theorem plus_ble_compat_l : forall n m p : nat,
  n <=? m = true -> (p + n) <=? (p + m) = true.
Proof.
  intros n m p. intros H. induction p as [|p' IHp'].
  -simpl. rewrite -> H. reflexivity.
  -simpl. rewrite -> IHp'. reflexivity. Qed.

Theorem S_nbeq_0 : forall n:nat,
  (S n) =? 0 = false.
Proof.
  intros n. destruct n as [|n'] eqn:E.
  - reflexivity.
  - reflexivity. Qed.

Theorem mult_1_l : forall n:nat, 1 * n = n.
Proof.
  intros n. destruct n as [|n'] eqn:E.
  - reflexivity.
  - simpl. rewrite <- plus_n_O. reflexivity. Qed.

Theorem all3_spec : forall b c : bool,
    orb
      (andb b c)
      (orb (negb b)
               (negb c))
  = true.
Proof.
  intros b c. destruct b as [] eqn:Eb.
  -destruct c as [] eqn:Ec.
    +reflexivity.
    +reflexivity.
  -destruct c as [] eqn:Ec.
    +reflexivity.
    +reflexivity.
Qed.


Theorem mult_S_l: forall m n : nat, (S n) * m = m * n + m.
Proof.
  intros m n. induction n as[|n' IHm']. 
  -simpl. rewrite -> mult_0_r. rewrite -> plus_comm. reflexivity.
  -simpl. assert (H1: m * (S n') = (S n') * m). 
      {rewrite -> mult_comm. reflexivity. }
  rewrite -> mult_comm. rewrite ->H1.  rewrite -> IHm'.
  rewrite -> plus_assoc. 
  assert (H2: m + m * n' = m * n' + m).
  { rewrite -> plus_comm. reflexivity. }
  rewrite <- double_plus. rewrite <- plus_comm. 
  rewrite -> double_plus. rewrite -> plus_assoc. 
  reflexivity. Qed.

Theorem mult_plus_distr_r : forall n m p : nat,
  (n + m) * p = (n * p) + (m * p).
Proof. 
  intros n m p. induction n as [|n' IHn'].
  - simpl. reflexivity.
  - simpl. rewrite -> IHn'. rewrite -> plus_assoc. reflexivity. Qed.


Theorem mult_assoc : forall n m p : nat,
  n * (m * p) = (n * m) * p.
Proof.
 intros n m p. induction n as [|n' IHn'].
  - simpl. reflexivity.
  - simpl. rewrite -> mult_plus_distr_r. 
    rewrite -> IHn'. reflexivity. Qed.

Theorem eqb_refl : forall n : nat,
  true = (n =? n).
Proof.
  intros n. induction n as [|n' IHn'].
  - reflexivity.
  - simpl. rewrite <- IHn'. reflexivity. Qed.

Theorem plus_swap' : forall n m p : nat,
  n + (m + p) = m + (n + p).
Proof.
  intros n m p. 
  rewrite -> plus_assoc. rewrite -> plus_assoc.
  replace (n + m) with (m + n).
  reflexivity.
  rewrite -> plus_comm. reflexivity. Qed.

Theorem bin_to_nat_pres_incr : forall b : bin,   
bin_to_nat (incr b) = S (bin_to_nat b).
Proof.
  intros b. induction b as [|b' IHb'|b'' IHb''].
  - reflexivity.
  - simpl. reflexivity.
  - simpl. rewrite -> IHb''. simpl.
    rewrite <- plus_n_Sm. reflexivity. Qed.


Definition manual_grade_for_binary_commute : option (nat*string) := None.




Fixpoint div_by_two (n m :nat) :nat := match m with
  |O => 0
  |S m' => if (2 * m)=?n then m else (div_by_two n m')
  end.

Fixpoint nat_to_bin (n:nat) : bin := match n with
  | O => Z
  | S n' => incr (nat_to_bin n')
  end.

Compute (nat_to_bin 89).

Theorem nat_bin_nat : forall n, bin_to_nat (nat_to_bin n) = n.
Proof.
  intros n. induction n as [|n' IHn'].
  - reflexivity.
  - simpl. rewrite -> bin_to_nat_pres_incr. rewrite -> IHn'. reflexivity. Qed.

Check (B).



Definition manual_grade_for_binary_inverse_a : option (nat*string) := None.

Definition manual_grade_for_binary_inverse_b : option (nat*string) := None.

Definition manual_grade_for_binary_inverse_c : option (nat*string) := None.

(*Inductive natprod : Type :=
| pair (n1 n2 : nat).

Check (pair 3 5).

Definition fst (p:natprod) :nat :=
  match p with
  |pair x y => x
  end.

Definition snd (p:natprod) :nat :=
  match p with
  |pair x y => y
  end.

Compute (snd (pair 3 5)).

Notation "( x , y )" := (pair x y).

Compute (fst (3,5)).

Definition fst' (p : natprod) : nat :=
  match p with
  | (x,y) => x
  end.

Definition snd' (p : natprod) : nat :=
  match p with
  | (x,y) => y
  end.

Definition swap_pair (p : natprod) : natprod :=
  match p with
  | (x,y) => (y,x)
  end.

Theorem surjective_pairing' : forall (n m : nat),
  (n,m) = (fst (n,m), snd (n,m)).
Proof.
  simpl. reflexivity. Qed.

Theorem surjective_pairing : forall (p : natprod),
  p = (fst p, snd p).
Proof.
  intros p. destruct p as [n m]. reflexivity. Qed.

Theorem snd_fst_is_swap : forall (p : natprod),
  (snd p, fst p) = swap_pair p.
Proof.
  intros p. destruct p as [n m]. simpl. reflexivity. Qed.

Theorem fst_swap_is_snd : forall (p : natprod),
  fst (swap_pair p) = snd p.
Proof.
  intros p. destruct p as [n m]. simpl. reflexivity. Qed.

Inductive natlist : Type :=
  | nil
  | cons (n:nat) (l:natlist).

Definition mylist := cons 1 (cons 2 (cons 3 nil)).

Notation " x :: l " := (cons x l) (at level 60, right associativity).

Notation "[]" := nil.

Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) .. ).

Fixpoint repeat (n count :nat) :natlist := 
  match count with
  | O => nil
  | S count' => n :: (repeat n count')
  end.

Fixpoint length (l :natlist) :nat :=
  match l with
  | [] => 0
  | n :: l' => S (length l')
  end.

Fixpoint app (l1 l2 :natlist) :natlist :=
  match l1 with
  | [] => l2
  | n::l1' => n::(app l1' l2)
  end.

Notation " x ++ y" := (app x y) (at level 60, right associativity).

Definition hd (default :nat) (l :natlist) :nat :=
  match l with
  | [] => default
  | n::l' => n
  end.

Definition tl (l :natlist) :natlist :=
  match l with
  | [] => []
  | n::l' => l'
  end.

Fixpoint nonzeros (l:natlist) : natlist :=
  match l with
  | [] => []
  | O::l' => nonzeros l'
  | n::l' => n::nonzeros l'
  end.

Fixpoint oddmembers (l:natlist) : natlist :=
  match l with
  | [] => []
  | n::l' => if oddb n then n :: oddmembers l' else oddmembers l'
  end.

Definition countoddmembers (l:natlist) : nat := length (oddmembers l).

Fixpoint alternate (l1 l2 : natlist) : natlist :=
  match l1, l2 with
  | [], _ => l2
  | _, [] => l1
  | n :: l1', m :: l2' => n :: m :: (alternate l1' l2')
  end.

Example test_alternate1:
  alternate [1;2;3] [4;5;6] = [1;4;2;5;3;6].
Proof. simpl. reflexivity. Qed.

Example test_alternate2:
  alternate [1] [4;5;6] = [1;4;5;6].
Proof. simpl. reflexivity. Qed.

Example test_alternate3:
  alternate [1;2;3] [4] = [1;4;2;3].
Proof. simpl. reflexivity. Qed.

Example test_alternate4:
  alternate [] [20;30] = [20;30].
Proof. simpl. reflexivity. Qed.

Definition bag := natlist.

Fixpoint count (v:nat) (s:bag) : nat :=
  match s with
  |[] => 0
  |n :: s'=> if n=?v then S (count v s') else count v s'
  end.

Example test_count1:              count 1 [1;2;3;1;4;1] = 3.
Proof. simpl. reflexivity. Qed.
Example test_count2:              count 6 [1;2;3;1;4;1] = 0.
Proof. simpl. reflexivity. Qed.


  
Definition add (v:nat) (s:bag) : bag := v::s.


Example test_add1:                count 1 (add 1 [1;4;1]) = 3.
Proof. reflexivity. Qed.
Example test_add2:                count 5 (add 1 [1;4;1]) = 0.
Proof. reflexivity. Qed.

Definition member (v:nat) (s:bag) : bool := negb (0 =? (count v s)).

Example test_member1:             member 1 [1;4;1] = true.
Proof. reflexivity. Qed.

Example test_member2:             member 2 [1;4;1] = false.
Proof. reflexivity. Qed.

Fixpoint remove_one (v:nat) (s:bag) : bag := 
  match s with
  |[] => []
  |n::s' => if n =? v then s' else n :: (remove_one v s')
  end.

Example test_remove_one1:
  count 5 (remove_one 5 [2;1;5;4;1]) = 0.
Proof. reflexivity. Qed.

Example test_remove_one2:
  count 5 (remove_one 5 [2;1;4;1]) = 0.
Proof. reflexivity. Qed.

Example test_remove_one3:
  count 4 (remove_one 5 [2;1;4;5;1;4]) = 2.
Proof. reflexivity. Qed.

Example test_remove_one4:
  count 5 (remove_one 5 [2;1;5;4;5;1;4]) = 1.
Proof. reflexivity. Qed.

Fixpoint remove_all (v:nat) (s:bag) : bag := 
  match s with
  | [] => []
  | n::s' => if n =? v then remove_all v s' else n::(remove_all v s')
  end.

Example test_remove_all1:  count 5 (remove_all 5 [2;1;5;4;1]) = 0.
Proof. reflexivity. Qed.
Example test_remove_all2:  count 5 (remove_all 5 [2;1;4;1]) = 0.
Proof. reflexivity. Qed.
Example test_remove_all3:  count 4 (remove_all 5 [2;1;4;5;1;4]) = 2.
Proof. reflexivity. Qed.
Example test_remove_all4:  count 5 (remove_all 5 [2;1;5;4;5;1;4;5;1;4]) = 0.
Proof. reflexivity. Qed.

Fixpoint subset (s1:bag) (s2:bag) : bool :=
  match s1 with
  | [] => true
  | n :: s1' => if negb (0 =? count n s2)
    then subset s1' (remove_one n s2)
    else false
end.

Example test_subset1:              subset [1;2] [2;1;4;1] = true.
Proof. simpl.  reflexivity. Qed.
Example test_subset2:              subset [1;2;2] [2;1;4;1] = false.
Proof. reflexivity. Qed.

Definition manual_grade_for_bag_theorem : option (nat*string) := None.

Theorem nil_app : forall l:natlist,
  [] ++ l = l.
Proof. reflexivity. Qed.

Theorem tl_length_pred : forall l:natlist,
  pred (length l) = length (tl l).
Proof.
  intros l. destruct l as [|l']eqn:E.
  - simpl. reflexivity.
  - simpl. reflexivity.
Qed.

Theorem app_assoc : forall l1 l2 l3 : natlist,
  (l1 ++ l2) ++ l3 = l1 ++ (l2 ++ l3).
Proof.
  intros l1 l2 l3.
  induction l1 as [|n l1' IHn'].
  - simpl. reflexivity.
  - simpl. rewrite -> IHn'. reflexivity. Qed.

Fixpoint rev (l:natlist) : natlist :=
  match l with
  | [] => []
  | n :: l' => (rev l') ++ [n]
  end.

Example test_rev1:            rev [1;2;3] = [3;2;1].
Proof. reflexivity.  Qed.
Example test_rev2:            rev nil = nil.
Proof. reflexivity.  Qed.

Theorem app_length : forall l1 l2 : natlist,
  length (l1 ++ l2) = (length l1) + (length l2).
Proof.
  intros l1 l2. induction l1 as [|n l' IHl'].
  - reflexivity.
  - simpl. rewrite -> IHl'. reflexivity. Qed.

Theorem rev_length : forall l : natlist,
  length (rev l) = length l.
Proof.
  intros l. induction l as [|n l' IHl'].
  - reflexivity.
  - simpl. rewrite -> app_length. rewrite -> IHl'. 
    simpl. rewrite -> plus_comm. rewrite -> plus_1_n.
    reflexivity. Qed.

Search rev.

Theorem app_nil_r : forall l : natlist,
  l ++ [] = l.
Proof.
  intros l. induction l as [|n l' IHl'].
  - reflexivity.
  - simpl. rewrite -> IHl'. reflexivity. Qed.

Theorem rev_app_distr: forall l1 l2 : natlist,
  rev (l1 ++ l2) = rev l2 ++ rev l1.
Proof.
  intros l1 l2. induction l1 as [|n l1' IHl1'].
  -simpl. rewrite -> app_nil_r. reflexivity.
  -simpl. rewrite -> IHl1'. rewrite -> app_assoc. reflexivity. Qed.

Theorem rev_involutive : forall l : natlist,
  rev (rev l) = l.
Proof.
  intros l. induction l as [|n l' IHl'].
  - reflexivity.
  -simpl. rewrite -> rev_app_distr. 
    rewrite -> IHl'. simpl. reflexivity. Qed.

Theorem app_assoc4 : forall l1 l2 l3 l4 : natlist,
  l1 ++ (l2 ++ (l3 ++ l4)) = ((l1 ++ l2) ++ l3) ++ l4.
Proof.
  intros l1 l2 l3 l4. 
  rewrite <- app_assoc.
  rewrite <- app_assoc. 
  reflexivity. Qed.

Lemma nonzeros_app : forall l1 l2 : natlist,
  nonzeros (l1 ++ l2) = (nonzeros l1) ++ (nonzeros l2).
Proof.
  intros l1 l2. induction l1 as [|n l1' IHl1'].
  - simpl. reflexivity.
  - destruct n as [|n']eqn:E.
    + simpl. rewrite -> IHl1'. reflexivity.
    + simpl. rewrite -> IHl1'. reflexivity. Qed.

Fixpoint eqblist (l1 l2 : natlist) : bool :=
  match l1, l2 with
  |[], [] => true
  |[], _ => false
  |_, [] => false
  |n::l1', m::l2' => if n =? m then eqblist l1' l2' else false
  end.



Example test_eqblist1 :
  (eqblist nil nil = true).
Proof. simpl. reflexivity. Qed.

Example test_eqblist2 :
  eqblist [1;2;3] [1;2;3] = true.
Proof. simpl. reflexivity. Qed.

Example test_eqblist3 :
  eqblist [1;2;3] [1;2;4] = false.
Proof. simpl. reflexivity. Qed.

Theorem eqblist_refl : forall l:natlist,
  true = eqblist l l.
Proof.
  intros l. induction l as [|n l' IHl'].
  - reflexivity.
  - simpl. rewrite <- IHl'. rewrite <- eqb_refl. reflexivity. Qed.

Theorem count_member_nonzero : forall (s : bag),
  1 <=? (count 1 (1 :: s)) = true.
Proof.
  intros s. reflexivity. Qed.


Theorem leb_n_Sn : forall n,
  n <=? (S n) = true.
Proof.
  intros n. induction n as [|n' IHn'].
  - reflexivity.
  - simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem remove_does_not_increase_count: forall (s : bag),
  (count 0 (remove_one 0 s)) <=? (count 0 s) = true.
Proof.
  intros s. induction s as [|n s' IHs'].
  - reflexivity.
  - simpl. destruct n as [|n'].
    + rewrite <- eqb_refl. rewrite -> leb_n_Sn. reflexivity.
    + simpl. rewrite -> IHs'. reflexivity. Qed.

Theorem rev_injective : forall (l1 l2:natlist),
  rev l1 = rev l2 -> l1 = l2.
Proof.
  intros l1 l2. intros H. 
  rewrite <- rev_involutive. 
  rewrite <- H.
  rewrite -> rev_involutive.
  reflexivity. Qed.

Definition manual_grade_for_rev_injective : option (nat*string) := None.

Fixpoint nth_bad (l:natlist) (n:nat) : nat :=
  match l with
  | nil => 42  (* arbitrary! *)
  | a :: l' => match n =? O with
               | true => a
               | false => nth_bad l' (pred n)
               end
  end.

Inductive natoption : Type :=
  | Some (n : nat)
  | None.

Fixpoint nth_error (l:natlist) (n:nat) : natoption :=
  match l with
  | [] => None
  | m::l' => match n with
              |O => Some m
              |S n' => nth_error l' n'
              end
  end.

Example test_nth_error1 : nth_error [4;5;6;7] 0 = Some 4.
Proof. reflexivity. Qed.
Example test_nth_error2 : nth_error [4;5;6;7] 3 = Some 7.
Proof. reflexivity. Qed.
Example test_nth_error3 : nth_error [4;5;6;7] 9 = None.
Proof. reflexivity. Qed.

Fixpoint nth_error' (l:natlist) (n:nat) : natoption :=
  match l with
  | nil => None
  | a :: l' => if n =? O then Some a
               else nth_error' l' (pred n)
  end.

Definition option_elim (d : nat) (o : natoption) : nat :=
  match o with
  | Some n' => n'
  | None => d
  end.

Definition hd_error (l : natlist) : natoption :=
  match l with
  | [] => None
  | n::l' => Some n
  end.

Example test_hd_error1 : hd_error [] = None.
Proof. reflexivity. Qed.

Example test_hd_error2 : hd_error [1] = Some 1.
Proof. reflexivity. Qed.

Example test_hd_error3 : hd_error [5;6] = Some 5.
Proof. reflexivity. Qed.

Theorem option_elim_hd : forall (l:natlist) (default:nat),
  hd default l = option_elim default (hd_error l).
Proof.
  intros l. destruct l as [|n l']eqn:E.
  - simpl. reflexivity.
  - simpl. reflexivity.
Qed.



Inductive id : Type :=
  | Id (n : nat).

Definition eqb_id (x1 x2 : id):bool :=
  match x1, x2 with
  | Id n1, Id n2 => n1 =? n2
  end.

Theorem eqb_id_refl : forall x, true = eqb_id x x.
Proof.
  intros x. destruct x. simpl. rewrite <- eqb_refl. reflexivity. Qed.
  


End NatList.

Module PartialMap.

Export NatList.

Inductive partial_map : Type :=
  | empty
  | record (i : id) (v : nat) (m : partial_map).

Definition update (d : partial_map)
                  (x : id) (value : nat) 
                  :partial_map :=
           record x value d.

Fixpoint find (x : id) (d : partial_map) : natoption :=
  match d with
  | empty => None
  | record x' v d' => if eqb_id x x' 
                      then Some v 
                      else find x d'
  end.

Theorem update_eq :
  forall (d : partial_map) (x : id) (v: nat),
    find x (update d x v) = Some v.
Proof.
  intros d x v. simpl. rewrite <- eqb_id_refl. reflexivity. Qed.

Theorem update_neq :
  forall (d : partial_map) (x y : id) (o: nat),
    eqb_id x y = false -> find x (update d y o) = find x d.
Proof. intros x y. Admitted.*)

(* Debut Cours V sur le polymorphisme *)

Inductive list (X:Type) :=
  |nil
  |cons (x : X) (l : list X).

Check list.

Check (nil nat).

Check (cons nat 3 (nil nat)).

Check nil.

Check cons.

Check (cons nat 2 (cons nat 1 (nil nat))).

Fixpoint repeat (X : Type) (x : X) (count : nat):list X:= 
  match count with
  |0 => nil X
  |S count' => cons X x (repeat X x count')
end.

Example test_repeat1 :
  repeat nat 4 2 = cons nat 4 (cons nat 4 (nil nat)).
Proof. reflexivity.  Qed.

Example test_repeat2 :
  repeat bool false 1 = cons bool false (nil bool).
Proof. reflexivity.  Qed.

Module MumbleGrumble.

Inductive mumble : Type :=
  | a
  | b (x : mumble) (y : nat)
  | c.

Inductive grumble (X:Type) : Type :=
  | d (m : mumble)
  | e (x : X).
  
(** Which of the following are well-typed elements of [grumble X] for
    some type [X]?  (Add YES or NO to each line.)
      - [d (b a 5)]
      - [e bool true]
      - [e mumble (b c 0)] *)
      
End MumbleGrumble.

Definition manual_grade_for_mumble_grumble : option (nat*string) := None.

(* Type Annotation Inference *)

Fixpoint repeat' X x count : list X :=
  match count with
  | 0        => nil X
  | S count' => cons X x (repeat' X x count')
  end.
  
Definition list123 :=
  cons nat 1 (cons nat 2 (cons nat 3 (nil nat))).

Definition list123' :=
  cons _ 1 (cons _ 2 (cons _ 3 (nil _))).

(* Implicit Arguments *)

Arguments nil {X}.
Arguments cons {X} _ _.
Arguments repeat {X} x count.

Definition list123'' := cons 1 (cons 2 (cons 3 nil)).

Inductive list' {X:Type} : Type :=
  | nil'
  | cons' (x : X) (l : list').
  
Fixpoint app {X : Type} (l1 l2 : list X)
             : (list X) :=
  match l1 with
  |nil => l2
  |cons x l1' => cons x (app l1' l2)
  end.
  
Fixpoint rev {X:Type} (l:list X) : list X := match l with
  |nil => nil
  |cons x l' => app (rev l') (cons x nil)
  end.
  
Fixpoint length {X : Type} (l : list X) : nat :=
  match l with
  |nil => O
  |cons x l' => S (length l')
  end.
  
Example test_rev1 :
  rev (cons 1 (cons 2 nil)) = (cons 2 (cons 1 nil)).
Proof. reflexivity.  Qed.

Example test_rev2:
  rev (cons true nil) = cons true nil.
Proof. reflexivity.  Qed.

Example test_length1: length (cons 1 (cons 2 (cons 3 nil))) = 3.
Proof. reflexivity.  Qed.

(* Supplying Type Arguments Explicitly *)

Fail Definition mynil := nil.

Definition mynil : list nat := nil.

Check @nil.

Definition mynil' := @nil nat.

Notation "x :: y" := (cons x y)(at level 60, right associativity).

Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y []) ..).
Notation "x ++ y" := (app x y)
                     (at level 60, right associativity).

Definition list123''' := [1; 2; 3].

Theorem app_nil_r : forall (X:Type), forall l:list X,
  l ++ [] = l.
Proof.
  intros X. intros l. induction l as [|x l' IHl'].
  -reflexivity.
  -simpl. rewrite -> IHl'. reflexivity. Qed.

Theorem app_assoc : forall A (l m n:list A),
  l ++ m ++ n = (l ++ m) ++ n.
Proof.
  intros A l m n. induction l as [|x l' IHl'].
  - simpl.  reflexivity.
  - simpl. rewrite -> IHl'. reflexivity. Qed. 

Lemma app_length : forall (X:Type) (l1 l2 : list X),
  length (l1 ++ l2) = length l1 + length l2.
Proof.
  intros X l1 l2. induction l1 as [|x l1' IHl1'].
  -reflexivity.
  -simpl. rewrite -> IHl1'. reflexivity. Qed.

Inductive prod (X Y : Type) : Type :=
| pair (x : X) (y : Y).

Arguments pair {X} {Y} _ _.

Notation "( x , y )" := (pair x y).

Notation "X * Y" := (prod X Y) : type_scope.

Definition fst {X Y : Type} (p : X * Y) : X :=
  match p with
  | (x, y) => x
  end.

Definition snd {X Y : Type} (p : X * Y) : Y :=
  match p with
  | (x, y) => y
  end.

Fixpoint combine {X Y : Type} (lx : list X) (ly : list Y)
           : list (X*Y) :=
  match lx, ly with
  | [], _ => []
  | _, [] => []
  | x :: tx, y :: ty => (x, y) :: (combine tx ty)
  end.

Fixpoint split {X Y : Type} (l : list (X*Y))
               : (list X) * (list Y) :=
    match l with
    |[ ] => ([ ],[])
    |cons (x,y) l' => (x::(fst (split l')), y::(snd (split l')))
end.
  

Example test_split:
  split [(1,false);(2,false)] = ([1;2],[false;false]).
Proof.
  reflexivity. Qed.

Module OptionPlayground.

Inductive option (X:Type) : Type :=
  | Some (x : X)
  | None.

Arguments Some {X} _.
Arguments None {X}.

End OptionPlayground.

Fixpoint nth_error {X : Type} (l : list X) (n : nat)
                   : option X :=
  match l with
  | [] => None
  | a :: l' => if n =? O then Some a else nth_error l' (pred n)
  end.

Example test_nth_error1 : nth_error [4;5;6;7] 0 = Some 4.
Proof. reflexivity. Qed.
Example test_nth_error2 : nth_error [[1];[2]] 1 = Some [2].
Proof. reflexivity. Qed.
Example test_nth_error3 : nth_error [true] 2 = None.
Proof. reflexivity. Qed.

Definition hd_error {X : Type} (l : list X) : option X :=
  match l with
  |[] => None
  |cons x l' => Some x end.


Check @hd_error.

Example test_hd_error1 : hd_error [1;2] = Some 1.
Proof. reflexivity. Qed.
Example test_hd_error2 : hd_error  [[1];[2]]  = Some [1].
Proof. reflexivity. Qed.

Definition doit3times {X:Type} (f:X->X) (n:X) : X :=
  f (f (f n)).

Check @doit3times.
(* ===> doit3times : forall X : Type, (X -> X) -> X -> X *)

Example test_doit3times: doit3times minustwo 9 = 3.
Proof. reflexivity.  Qed.

Example test_doit3times': doit3times negb true = false.
Proof. reflexivity.  Qed.

Fixpoint filter {X:Type} (test: X->bool) (l:list X)
                : (list X) :=
  match l with
  | []     => []
  | h :: t => if test h then h :: (filter test t)
                        else       filter test t
  end.

Example test_filter1: filter evenb [1;2;3;4] = [2;4].
Proof. reflexivity.  Qed.

Definition length_is_1 {X : Type} (l : list X) : bool :=
  (length l) =? 1.

Example test_filter2:
    filter length_is_1
           [ [1; 2]; [3]; [4]; [5;6;7]; []; [8] ]
  = [ [3]; [4]; [8] ].
Proof. reflexivity.  Qed.

Definition countoddmembers' (l:list nat) : nat :=
  length (filter oddb l).

Example test_countoddmembers'1:   countoddmembers' [1;0;3;1;4;5] = 4.
Proof. reflexivity.  Qed.
Example test_countoddmembers'2:   countoddmembers' [0;2;4] = 0.
Proof. reflexivity.  Qed.
Example test_countoddmembers'3:   countoddmembers' nil = 0.
Proof. reflexivity.  Qed.

Example test_anon_fun':
  doit3times (fun n => n * n) 2 = 256.
Proof. reflexivity.  Qed. 

Example test_filter2':
    filter (fun l => (length l) =? 1)
           [ [1; 2]; [3]; [4]; [5;6;7]; []; [8] ]
  = [ [3]; [4]; [8] ].
Proof. reflexivity.  Qed.

Definition filter_even_gt7 (l : list nat) : list nat := filter evenb (filter (leb 7) l).

Example test_filter_even_gt7_1 :
  filter_even_gt7 [1;2;6;9;10;3;12;8] = [10;12;8].
Proof. reflexivity. Qed.

Example test_filter_even_gt7_2 :
  filter_even_gt7 [5;2;6;19;129] = [].
Proof. reflexivity. Qed.

Definition partition {X : Type}
                     (test : X -> bool)
                     (l : list X)
                   : list X * list X :=
    (filter test l , filter ((fun i => negb (test i)))  l) .

Example test_partition1: partition oddb [1;2;3;4;5] = ([1;3;5], [2;4]).
Proof. reflexivity. Qed.
Example test_partition2: partition (fun x => false) [5;9;0] = ([], [5;9;0]).
Proof. reflexivity. Qed.

Fixpoint map {X Y: Type} (f:X->Y) (l:list X) : (list Y) :=
  match l with
  | []     => []
  | h :: t => (f h) :: (map f t)
  end.

Example test_map1: map (fun x => plus 3 x) [2;0;2] = [5;3;5].
Proof. reflexivity.  Qed.

Example test_map2:
  map oddb [2;1;2;5] = [false;true;false;true].
Proof. reflexivity.  Qed.

Example test_map3:
    map (fun n => [evenb n;oddb n]) [2;1;2;5]
  = [[true;false];[false;true];[true;false];[false;true]].
Proof. reflexivity.  Qed.


Fixpoint flat_map {X Y: Type} (f: X -> list Y) (l: list X)
                   : (list Y) :=
  match l with
  |[] => []
  |cons x l' => app (f x) (flat_map f l') end.

Example test_flat_map1:
  flat_map (fun n => [n;n;n]) [1;5;4]
  = [1; 1; 1; 5; 5; 5; 4; 4; 4].
Proof. reflexivity. Qed.

Definition option_map {X Y : Type} (f : X -> Y) (xo : option X)
                      : option Y :=
  match xo with
    | None => None
    | Some x => Some (f x)
  end.

(* Fold *)

Fixpoint fold {X Y: Type} (f: X->Y->Y) (l: list X) (b: Y)
                         : Y :=
  match l with
  | nil => b
  | h :: t => f h (fold f t b)
  end.

Check (fold andb).

Example fold_example1 :
  fold mult [1;2;3;4] 1 = 24.
Proof. reflexivity. Qed.

Example fold_example2 :
  fold andb [true;true;false;true] true = false.
Proof. reflexivity. Qed.

Example fold_example3 :
  fold app  [[1];[];[2;3];[4]] [] = [1;2;3;4].
Proof. reflexivity. Qed.

Definition manual_grade_for_fold_types_different : option (nat*string) := None.

(* Functions That Construct Functions *)

Definition constfun {X: Type} (x: X) : nat->X :=
  fun (k:nat) => x.

Definition ftrue := constfun true.

Example constfun_example1 : ftrue 0 = true.
Proof. reflexivity. Qed.

Example constfun_example2 : (constfun 5) 99 = 5.
Proof. reflexivity. Qed.

Check plus.

Definition plus3 := plus 3.
Check plus3.

Example test_plus3 :    plus3 4 = 7.
Proof. reflexivity.  Qed.
Example test_plus3' :   doit3times plus3 0 = 9.
Proof. reflexivity.  Qed.
Example test_plus3'' :  doit3times (plus 3) 0 = 9.
Proof. reflexivity.  Qed.

Module Exercises.

Definition fold_length {X : Type} (l : list X) : nat :=
  fold (fun _ n => S n) l 0.

Example test_fold_length1 : fold_length [4;7;0] = 3.
Proof. reflexivity. Qed.

Theorem fold_length_correct : forall X (l : list X),
  fold_length l = length l.
Proof.
  intros X l. induction l as [|x l' IHl'].
  - reflexivity.
  - assert (H: fold_length (x :: l') = S (fold_length l')).
  {reflexivity. }
  simpl. rewrite -> H. rewrite -> IHl'. reflexivity. Qed.

(* had to done fold_map *)
(*

definion multiple x y := fold plus (repeat x y)

definition power x y := fold multiple (repeat x y)

*)




