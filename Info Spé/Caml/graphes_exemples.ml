(* Deux graphes d'exemple *)

let ex01 = 
[|[(1,1);(2,3)];[(0,1);(2,1);(3,2)];[(0,3);(1,1);(4,4)];
  [(1,2);(4,2);(5,6)];[(2,4);(3,2);(5,2)];[(3,6);(4,2)]|]

let ex02 = 
[|[(1,6);(2,9)];[(0,6);(2,5);(3,8);(6,6)];[(0,9);(1,5);(3,4);(4,8);(5,7)];
  [(1,8);(2,4);(5,4);(6,5)];[(2,8);(5,9);(7,4)];[(2,7);(3,4);(4,9);(6,3);(7,10)];
  [(1,6);(3,5);(5,3);(7,6)];[(4,4);(5,10);(6,6)]|];;

  type chemin = int list;;