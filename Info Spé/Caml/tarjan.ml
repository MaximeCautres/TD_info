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
  let (component : int list list ref) = ref [] in
  let n = Array.length graph in
  let state = Array.make (n) (-1) in
  let low_link = Array.make (n) (-1) in
  let stack = ref [] in
  let k = ref 0 in
  let rec isin e l = match l with
    |[] -> false
    |i::l' -> if i = e then true else isin e l' in
  let rec pop e l stock = match l with
    |[] -> stock, []
    |ele::l' -> if ele = e then (stock, l) else pop e l' (stock @ [ele])  in
  let rec dfs_mod s' =
    if state.(s') = -1 then begin
        state.(s') <- !k;
        k := !k + 1;
        stack := !stack @ [s'];
        low_link.(s') <- state.(s');
        for i = 0 to Array.length graph.(s') - 1 do
          let value = dfs_mod graph.(s').(i) in
          if value <> -1 then low_link.(s') <- (min low_link.(s') value)
        done;
        if low_link.(s') = state.(s')
        then begin
            let newstack, compo = pop s' !stack [] in
            component := compo :: !component;
            stack := newstack;
            -1;
          end
        else low_link.(s')
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

tarjan graph2;;


type graphe_l = int list array;;

let tarjanl (graph : graphe_l) =
  
  let composante = ref [] in
  let n = Array.length graph in
  let state = Array.make (n) (-1) in
  let low_link = Array.make (n) (-1) in
  let stack = ref [] in
  let k = ref 0 in
  
  let rec inlist s l = match l with
    |[] -> false
    |e::l' -> if e = s then true else inlist s l' in
  
  let rec extract s l comp = match l with
    |[] -> l, comp
    |e::l' -> let n_comp = e :: comp in if e = s then (l', n_comp) else extract s l' n_comp in
  
  let rec dfs_aux s =
    if state.(s) = -1 then
      begin
        let rec reccur_dfs l s = match l with
          |[] -> ()
          |s' :: l' -> begin
              let value = (dfs_aux s') in 
        if value <> -1 then low_link.(s) <- min (low_link.(s)) (value);
        reccur_dfs l' s  end in
        state.(s) <- !k;
        low_link.(s) <- !k;
        k := !k + 1;
        stack := s :: !stack ;
        reccur_dfs (graph.(s)) s;
        if state.(s) = low_link.(s)
        then
          begin
            let n_stack, n_compo = extract s !stack [] in
            stack := n_stack;
            composante := n_compo :: !composante;
            -1 ;
          end
        else low_link.(s) ;
      end 
    else
      begin
        if inlist s !stack then state.(s) else -1 ;
      end in

  let trash = ref 0 in
  for s = 0 to n - 1 do
    if state.(s) = -1 then trash := dfs_aux s ;
  done;
  !composante;;


let graphl2 = [|[1];[2];[3;4];[0;9];[5];[6;8];[0;7];[4];[];[10];[9]|];;

tarjanl graphl2;;

type poids = P of int | Infini ;;

type matrice_pond = int array array;;

let plusp a b = match a, b with
  |P a', P b' -> P (a' + b')
  |_, _ -> Infini;;

let minp a b = match a, b with
  |P a', P b' -> P (min a' b')
  |P a', Infini -> P a'
  |Infini, P b' -> P b'
  |_, _ -> Infini ;;

let convert (graph : (int*int) list array) =
  let n = Array.length graph in
  let ngraph = Array.make n [||] in

  for s = 0 to n - 1 do ngraph.(s) <- Array.make n Infini done;

  let rec aux l s = match l with
    |[] -> ()
    |(s',p)::l' -> ( ngraph.(s).(s') <- P p; aux l' s) in

  for s = 0 to n-1 do aux graph.(s) s done;
  ngraph;;
    
let vgraph = [|[(1,4);(3,1)];[(3, 10)];[(1, 5); (1, 15)];[(2, 19)]|];;

convert vgraph ;;
