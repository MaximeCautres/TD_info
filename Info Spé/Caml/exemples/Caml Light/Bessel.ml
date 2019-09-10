#open "graphics";;
open_graph " 960x600";;
set_window_title "Fonctions de Bessel";;

let orx, ory = 480., 300.;;
let ux, uy = 60., 225.;;
let X a = int_of_float (orx +. a *. ux);;
let Y b = int_of_float (ory +. b *. uy);;

let axes x1 x2 y1 y2 e =
   moveto (X x1) (Y 0.); lineto (X x2) (Y 0.);
   moveto (X 0.) (Y y1); lineto (X 0.) (Y y2);
   moveto (X 1.) (Y (-. e /. uy)); lineto (X 1.) (Y (e /. uy));
   moveto (X (-. e /. ux)) (Y 1.); lineto (X (e /. ux)) (Y 1.);
;;

(* ------------------------------------- Opérations sur les vecteurs ------------------------------------*)

let prefix </ x y =
   let n = vect_length x in
      for i = 0 to n - 1 do
         x.(i) <- y.(i)
      done;
;;

let copy_vect y =
   let n = vect_length y in
      let z = make_vect n 0. in
         z </ y;
         z
;;

let prefix +/ y1 y2 =
   let n = vect_length y1 in
      let y = make_vect n 0. in
         for i = 0 to n - 1 do
            y.(i) <- y1.(i) +. y2.(i)
         done;
         y
;;

let prefix */ a y =
   let n = vect_length y in let z = make_vect n 0. in
         for i = 0 to n - 1 do
            z.(i) <- a *. y.(i)
         done;
         z
;;

(* ---------- Résolution numérique d'une équation différentielle du premier ordre vectorielle ---------- *)
(* ----------------------------- y' = f(x,y) par la méthode de Runge-Kutta ----------------------------- *)

let RK h f x y =
   let K1 = f !x y in
      let K2 = f (!x +. h /. 2.) (y +/ h /. 2. */ K1) in
         let K3 = f (!x +. h /. 2.) (y +/ h /. 2. */ K2) in
            let K4 = f (!x +. h) (y +/ h */ K3) in
               x := !x +. h;
               y </ y +/ h /. 6. */ (K1 +/ 2. */ (K2 +/ K3) +/ K4)
;;

(* ---------- Représentation graphique sur le segment [x0 , x0 + n h] de la p-ième projection ---------- *)
(* ------------------ de la solution du problème de Cauchy (y' = f(x,y) y(x0) = y0) -------------------- *)

let drawLine next x0 y0 n p =
   let u = ref x0 and v = copy_vect y0 in
      for i = 0 to n do
         moveto (X !u) (Y v.(p));
         next u v;
         lineto (X !u) (Y v.(p))
      done
;;

let graphRK h f x0 y0 n p = drawLine (RK h f) x0 y0 n p;;

(* -------------- Calcul des fonctions de Bessel par leurs développements en série entière ------------- *)

let Bessel n x =
   let eps = 1e-15 and u = ref 1. in
      for k = 1 to n do
         u := !u *. x /. 2. /. float_of_int k
      done;
      let s = ref 0. and k = ref 1 and v = ref 1. in
         while abs_float !v > eps do
            s := !s +. !v;
            v := !v *. (-. x *. x /. 4. /. float_of_int !k /. float_of_int (n + !k));
            k := !k + 1
         done;
         !u *. !s
;;

(* ----- tracé sur [-10, 10] des 10 premières courbes par la méthode de Runge-Kutta (courbes noires) ---- *)
(* ---------------- à partir de l'équation différentielle y'' + y'/x + (1 - n^2/x^2)y = 0 -------------- *)
(* ----------------- amorçage à x = e ou -e à partir des développements en série entière --------------- *)

let f n x y = if n > 0 then [|y.(1); (float_of_int (n * n) /. x /. x -. 1.) *. y.(0) -. y.(1) /. x|]
   else [|y.(1); -. y.(0) -. y.(1) /. x|]
and dsym h g x = (g (x +. h) -. g (x -. h)) /. (2. *. h)
in
   let h = 0.001 and m = 10000 and e = 0.01 in
      let a = h *. float_of_int m in
         axes (-. a) a (-. 2.) 2. 5.;
         let Bessel' n x = dsym (h /. 10.) (Bessel n) x in
            for n = 0 to 9 do
               graphRK h (f n) e [|Bessel n e; Bessel' n e|] m 0;
               graphRK (-. h) (f n) (-. e) [|Bessel n (-. e); Bessel' n (-. e)|] m 0
            done
;;

(* ---------------- mêmes tracés à partir des développements en série entière (en rouge) --------------- *)

let next h f x y = x := !x +. h; y.(0) <- f (!x)
in
   let graphe h f x0 y0 n = drawLine (next h f) x0 y0 n 0 (* représentation de f sur [x0, x0 + n h] *)
   in
      set_color red;
      for i = 0 to 9 do
         let y = make_vect 1 ((Bessel i) 0.) in
            graphe 0.1 (Bessel i) 0. y 100; (* sur [0, 10] *)
            graphe (-. 0.1) (Bessel i) 0. y 100 (* sur [-10, 0] *)
      done;
      set_color black
;;
