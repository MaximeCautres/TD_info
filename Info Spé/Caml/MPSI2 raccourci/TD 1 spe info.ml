(* EXERCICE 1 *)

1=2;;
'a'='b';;
1=2 || 1<2;;
1<>2 || not (1 = 2);; (* ne pas oublier les associativité *)
1 = 1 && 2 = 2;; (* pour les comparaison deux signe *)
2+4;;
2*4;;
2. *. 4.;; (*ne pas oublier les points pour les operation sur le type reel *)
2/4;;
2./.4.;;
2.*.4.;;
exp(1.)+.cos(2.);; (*  tjrs le probleme de point sur les nombre pour signaler leur type*)
[|1;2;3|].(2);; (*ressort la 3eme valeur de la liste ci contre *)
[|1;2;3|].(2);;
[|1,2,3|].(0);; (* donne les type des valeur de la liste *)
[|1;'a';1a|];; (* 1a n'est pas un type connu , ne reconnait pas le string *)
[1;2;3].(0);; (* le type n'est pas celui attendu par l'indentation *)
List.hd[1;2;3];; (* donne le premier element *)
List.tl[1;2;3];; (* donne tout les restant a partir du second *)
0::[1;2;3];; (* incertion au debut de la list *)

(* exercice 2 *)
let x = 1 in let x = 2 in x;; (* utiliser tout les variable declare dans la fonction *)
x;; (* x n'a pas ete declarer comme une variable global, il n'est donc pas acessible en dehors de la fonction *)
let x=1 ;; (* declaration gloabl de x ce qui le rend accessible, espace memoire alouer *)
x;; 
3*x where x = 2;; (* il n'est pas possible d'utiliser des element de fonction en dehors d'une fonction*)
let(x,y)=(1,2)in x*y;; (* definition de x et de y par un couple en global plus de leur produit*)
let x = 1 and y = 2 in x*y;;(* definition global de x et y independemment une de l'autre en global *)
let f1 = function x -> x*x;;
let f2 = fun x -> x*x;;
f2 4;; (* syntaxe de la l'utilisatio des fonction *)
let f3 x = x*x;;
f3 4;;
let p1(x,y)=x*y;;
let p2 x y = x*y;;
p2 2 3;;
let f1 = p2 4;;
f1 6;;
let p3 = function(x,y)->x*y;;
p3 (3,4);;
let p4 = function x y -> x*y;; (*error, you need to use un tuple *)
let x = 1;;
let f = fun y ->x+y;;
f(3);;
let x=4;;
f(3);;
let x=0;;
x := x + 1;;
let x = ref 0;; (* pour faire l'affectation *)
x:f(3);;
x:= !x+1;;
x;;

(* Exercice 3 *)

let a = let a = 3 and b = 2 in let a = a + b and b = a - b in a-a;;
let b = 2 in a-b*b;;

(* exercice 4 *)

(* 1 (int-> int)-> int
2 int -> int -> int
3 int -> int -> int
4 int -> (int -> int ) -> int *)

let gi x = 2*x;;
let g gi = x;;
let f(x) = x + 1;;
let g 2 = fun f -> f(1)+2;; (* rep 1*)
let f x y =x+y;; (* rep 2-3 *)
let x = 1;;
let g x = fun f -> f(x+1)+2;; (* rep 4 *)

(* exercice 5 
1 (int ->int-> int)-> int-> int->int
*)
fun f x+5 y-6 -> f x y;;

