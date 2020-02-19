let u = Array.make 39999 1;;

let remplissage () =
  for k = 1 to 39998 do
    u.(k) <- (15731 * u.(k-1)) mod 32003
  done;;

remplissage ();;

u.(1000) ;;

let grapheG m n =
  let graphe = Array.make_matrix n n 0 in
  for i = 0 to n-1 do
    for j = i + 1 to n-1 do
      if u.(1 + i + (j*(j - 1))/2) mod m = 0
      then (graphe.(i).(j) <- 1; graphe.(j).(i) <- 1)
    done;
  done;
  graphe;;

let nombre_arrete graphe =
  let n = Array.length graphe and count = ref 0 in
  for i = 0 to n -1 do
    for j = i + 1 to n - 1 do
      if graphe.(i).(j) = 1 then count := 1 + !count
    done ;
  done ;
  count;;

nombre_arrete (grapheG 10 50)

let approach graphe =
  let n = Array.length graphe in
  let c = Array.make n false in
  for i = 0 to n-1 do
    let j = ref 0 in
    while !j < n && (graphe.(!j).(i) = 0 || c.(!j) = true) do
      j := 1 + !j
    done;
    if !j <> n then c.(i) <- true
  done;
  c;;

let cardinal table =
  let n = Array.length table in
  let count = ref 0 in
  for k = 0 to n - 1 do
    if table.(k) then count := 1 + !count
  done ;
  !count;;

cardinal (approach (grapheG 50 250));;

let degre graphe =
  let n = Array.length graphe in
  let l = Array.make n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if graphe.(i).(j) == 1 then l.(i) <- l.(i) + 1
    done;
  done ;
  l;;


let maximum l =
  let maxi = ref l.(0) in
  for i = 1 to Array.length(l) - 1 do
    if l.(i) > !maxi then maxi := l.(i)
  done ;
  !maxi;;

maximum (degre (grapheG 10 50));;


let maxi (l : int array) =
  let ind  = ref 0 and ma = ref l.(0) in
  for i = 1 to Array.length(l) - 1 do
    if l.(i) > !ma then (ma := l.(i); ind := i)
  done ;
  !ind;;

let glouton graphe =
  let n = Array.length graphe in
  let c = Array.make n false in
  let deg = degre graphe in
  for k = 0 to n - 1 do
    let ind = maxi deg in
    deg.(ind) <- 0;
    c.(ind) <- true;
    for j = 0 to n-1 do
      graphe.(ind).(j) <- 0;
      if graphe.(j).(ind) = 1 then begin
        deg.(j) <- deg.(j) - 1;
        graphe.(j).(ind) <- 0 end
    done;
  done;
  c;;
    
cardinal (glouton (grapheG 360 250))

let valider graphe l =
  let n = Array.length graphe in
  let state = Array.make n false in
  for i = 0 to n - 1 do
    if l.(i) then
      state.(i) <- true;
      for j = 0 to n - 1 do
        if graphe.(i).(j) = 1 then state.(j) <- true
      done;
  done;
  let stat = ref true in
  for i = 0 to n - 1 do
    stat := !stat && state.(i)
  done;
  !stat ;;
    
valider (grapheG 360 250) (glouton(grapheG 360 250));;
