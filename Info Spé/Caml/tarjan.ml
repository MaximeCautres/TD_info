type 'a tree = N of 'a tree * 'a * 'a tree | F;;

let rec depth arbre = match arbre with
  |F -> -1
  |N(fg,_,fd) -> 1 + max (depth fg) (depth fd);;

let b1 = N(F, 3, F);;
let b2 = N(F, 4, F);;
let b3 = N(b1, 6, F);;
let b4 = N(b3, 1, b2);;

depth b4;;

let rec maxi arbre = match arbre with
  |F -> failwith "arbre vide, pas de max"
  |N(F,a, F) -> a
  |N(tg, a, F) -> max (maxi tg) a
  |N(F, a, td) -> max (maxi td) a
  |N(tg, a, td) -> max (max a (maxi tg)) (maxi td);;

type graphe = int array array;;

let dfs (graph : graphe) (s : int) =
  let n = Array.length graph in
  let state = Array.make n true in
  let l = ref [] in
  let rec dfs_aux s' =
    if state.(s') then begin
        l := !l @ [s'];
        state.(s') <- false;
        for i = 0 to Array.length graph.(s') - 1 do
          dfs_aux graph.(s').(i)
        done;
      end;
  in
  dfs_aux s;
  !l;;

let graph = [|[|1|];[|2;3;5|];[|0;3|];[|4|];[|3|];[|4|]|];;

dfs graph 0;;

let bfs (graph : graphe) (s : int) =
  let n = Array.length graph in
  let state = Array.make n true in
  state.(s) <- false;
  let fill = ref [s] in
  let rec bfs_aux fill = match !fill with
    |[] -> []
    |s'::_ -> (for i = 0 to Array.length graph.(s') -1 do
                    if state.(graph.(s').(i)) then begin
                        fill := !fill @ [graph.(s').(i)] ;
                        state.(graph.(s').(i) ) <- false;
                      end;
                   done;
               s':: (bfs_aux (ref (List.tl !fill)))) in
  bfs_aux fill;;
               

bfs graph 0;;

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l;;

let print_array t =
  for i = 0 to pred (Array.length t) do
    print_int t.(i) ;
    print_string " "
  done
;;

let tarjan (graph : graphe) =
  let (component : int list list ref) = ref [[]] in
  let n = Array.length graph in
  let state = Array.make (n) (-1) in
  let time = Array.make (n) (-1) in
  let stack = ref [] in
  let k = ref 0 in
  let rec isin e l = match l with
    |[] -> false
    |i::l' -> if i = e then true else isin e l' in
  let rec pop e l stock = match l with
    |[] -> stock, []
    |ele::l' -> if ele = e then (stock, l) else pop e l' (stock @ [ele])  in
  let rec dfs_mod s' =
    print_string "000000000000000000000000000000000000000000000000000000"; print_newline ();
    print_string "s = "; print_int s'; print_newline ();
    print_string "state = "; print_array state; print_newline ();
    print_string "time = "; print_array time; print_newline ();
    print_string "stack = "; print_list !stack; print_newline ();
    if state.(s') = -1 then begin
        state.(s') <- !k;
        k := !k + 1;
        stack := !stack @ [s'];
        let etat = ref true in
        for i = 0 to Array.length graph.(s') - 1 do
          let value = dfs_mod graph.(s').(i) in
          if value <> -1 then ( time.(s') <- (min state.(s') value); etat := false)
        done;
        print_string "1111111111111111111111111111111111111111111111111111111"; print_newline ();
        print_string "s = "; print_int s'; print_newline ();
        print_string "state = "; print_array state; print_newline ();
        print_string "time = "; print_array time; print_newline ();
        print_string "stack = "; print_list !stack; print_newline ();
        if time.(s') = state.(s') || !etat 
        then begin
            let (newstack, (compo : int list)) = pop s' !stack [] in
            component := compo :: !component;
            stack := newstack;
            -1;
          end
        else time.(s')
      end
    else begin
        if isin s' !stack
        then
          state.(s')
        else
          -1
      end
  in
  let trash = ref 0 in
  for s = 0 to n - 1 do
    if state.(s) = -1 then trash := dfs_mod s
  done;
  !component;;
        
        
      
  
                            
                        
tarjan graph;;

let graph2 = [|[|1|];[|2|];[|3;4|];[|0;9|];[|5|];[|6;8|];[|0;7|];[|4|];[||];[|10|];[|9|]|];;

etarjan graph2;;
