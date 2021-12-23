(* Day 21 *)
(* On réarrange des crevettes. *)

(* Leftist heaps *)

type 'a abr =
  | E
  | N of {
      value : 'a;
      score : int;
      rank : int;
      left : 'a abr;
      right : 'a abr;
    }

let rank = function
  | E -> 0
  | N { rank } -> rank
;;

let make x s a b =
  if rank a >= rank b then
    N
      {
        value = x;
        score = s;
        rank = 1 + rank b;
        left = a;
        right = b;
      }
  else
    N
      {
        value = x;
        score = s;
        rank = 1 + rank a;
        left = b;
        right = a;
      }
;;

let rec merge t1 t2 =
  match (t1, t2) with
  | t, E | E, t -> t
  | N s1, N s2 ->
      if s1.score <= s2.score then
        make s1.value s1.score s1.left (merge s1.right t2)
      else
        make s2.value s2.score s2.left (merge t1 s2.right)
;;

let insert v s t =
  merge
    (N
       {
         value = v;
         score = s;
         rank = 1;
         left = E;
         right = E;
       })
    t
;;

let extract = function
  | E -> None
  | N s -> Some (s.value, s.score, merge s.left s.right)
;;

let grid = Array.make_matrix 5 11 0

let start_config =
  [|
    (* 1 *)
    (6, 3);
    (6, 4);
    (8, 2);
    (8, 4);
    (* 2 *)
    (2, 4);
    (4, 1);
    (4, 3);
    (6, 2);
    (* 3 *)
    (2, 1);
    (4, 2);
    (4, 4);
    (8, 3);
    (* 4 *)
    (2, 2);
    (2, 3);
    (6, 1);
    (8, 1);
  |]
;;

let set_grid config =
  for i = 0 to 10 do
    for j = 0 to 4 do
      grid.(j).(i) <- 0
    done
  done;
  for i = 0 to 15 do
    let x, y = config.(i) in
    grid.(y).(x) <- (i / 4) + 1
  done
;;

let print_config c =
  let process x y =
    if grid.(y).(x) = 0 then
      print_char '.'
    else
      print_int grid.(y).(x)
  in

  set_grid c;
  for i = 0 to 10 do
    process i 0
  done;
  print_newline ();
  for j = 1 to 4 do
    print_string "  ";
    process 2 j;
    print_string " ";
    process 4 j;
    print_string " ";
    process 6 j;
    print_string " ";
    process 8 j;
    print_newline ()
  done;
  print_newline ()
;;

let stack = ref E
let costs = [| 1; 10; 100; 1000 |]

let print_states () =
  let rec aux = function
    | E -> ()
    | N { value; score; left; right } ->
        Printf.printf "Score: %d\n" score;
        print_config (fst value);
        aux left;
        aux right
  in
  aux !stack
;;

exception Stop

(* Test if one can go from x1 to x2 on the top line *)
let can_reach x1 x2 =
  try
    if x1 < x2 then begin
      for x = x1 + 1 to x2 do
        if grid.(0).(x) <> 0 then raise Stop
      done;
      true
    end
    else if x1 > x2 then begin
      for x = x2 to x1 - 1 do
        if grid.(0).(x) <> 0 then raise Stop
      done;
      true
    end
    else
      false
  with Stop -> false
;;

let is_destination config =
  try
    for i = 0 to 15 do
      let x, y = config.(i) in
      if y = 0 || x <> 2 * ((i / 4) + 1) then raise Stop
    done;
    true
  with Stop -> false
;;

let tri tab deb fin =
  for i = deb + 1 to fin do
    let v = tab.(i)
    and j = ref (i - 1) in
    while !j >= deb && tab.(!j) > v do
      tab.(!j + 1) <- tab.(!j);
      decr j
    done;
    tab.(!j + 1) <- v
  done
;;

let final_position x y =
  let t = x / 2 in
  y > 0 (* Pas sur la ligne du haut *)
  && grid.(y).(x) = t (* Dans la bonne colonne *)
  &&
  let r = ref true in
  for i = y + 1 to 4 do
    (* Il n'y a que de bonnes valeurs en dessous. *)
    if grid.(i).(x) <> t then r := false
  done;
  !r
;;

let can_move x y =
  (not (final_position x y))
  &&
  (* Peut sortir *)
  let r1 = ref true in
  for i = 1 to y - 1 do
    if grid.(i).(x) <> 0 then r1 := false
  done;
  !r1
;;

let heuristic conf =
  let score = ref 0 in
  for i = 0 to 15 do
    let t = i / 4 in
    let x, y = conf.(i) in
    if not (final_position x y) then
      score :=
        !score
        + costs.(t)
          * (y + 1
            +
            if x = 2 * (t + 1) then
              2
            else
              abs ((2 * (t + 1)) - x))
  done;
  !score
;;

let move i nx ny config cost =
  let ox, oy = config.(i) in
  let new_config = Array.copy config in
  new_config.(i) <- (nx, ny);
  (* assert (grid.(ny).(nx) = 0 && grid.(oy).(ox) = 1 + (i / 4)); *)
  grid.(ny).(nx) <- (i / 4) + 1;
  grid.(oy).(ox) <- 0;
  (* On classe à cause des permutations possibles. *)
  tri new_config (i / 4 * 4) ((i / 4 * 4) + 3);
  (* Coût du mouvement *)
  let delta =
    costs.(i / 4) * (abs (nx - ox) + abs (ny - oy))
  in
  stack :=
    insert
      (new_config, cost + delta)
      (cost + delta + heuristic new_config)
      !stack;
  grid.(ny).(nx) <- 0;
  grid.(oy).(ox) <- (i / 4) + 1
;;

let h = Hashtbl.create 1000

(* Peut-on mettre un amphipode de façon définitive
   dans la colonne x et si oui, jusqu'où ? *)
let test_col x =
  let y = ref 1 in
  while !y <= 4 && grid.(!y).(x) = 0 do
    incr y
  done;
  let r = ref true in
  for i = !y to 4 do
    if grid.(i).(x) <> x / 2 then r := false
  done;
  if !r then Some (!y - 1) else None
;;

let process_state config cost =
  set_grid config;
  for i = 0 to 15 do
    (* try to move i *)
    let x, y = config.(i)
    and t = (i / 4) + 1 in
    if y >= 1 && can_move x y then
      List.iter
        (fun xx ->
          if can_reach x xx then move i xx 0 config cost)
        [ 0; 1; 3; 5; 7; 9; 10 ];
    if
      (* grande salle *)
      y = 0 && can_reach x (2 * t)
      (* && grid.(1).(2 * t) = 0 *)
    then
      match test_col (2 * t) with
      | None -> ()
      | Some y -> move i (2 * t) y config cost
  done
;;

let rec dijkstra () =
  match extract !stack with
  | None -> failwith "Dijkstra"
  | Some ((conf, cost), _, stack') ->
      stack := stack';
      if not (Hashtbl.mem h conf) then begin
        Hashtbl.add h conf ();
        (* Printf.printf "Score: %d\n" cost;
           print_config conf; *)
        if is_destination conf then
          cost
        else begin
          process_state conf cost;
          dijkstra ()
        end
      end
      else
        dijkstra ()
;;

let _ =
  stack := insert (start_config, 0) 0 E;
  Hashtbl.reset h;
  Printf.printf "%d\n" (dijkstra ())
;;
