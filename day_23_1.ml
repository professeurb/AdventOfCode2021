(* Day 23 *)
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

(*

 01234567890 
#############
#..2.4.6.8..# 0
###C#B#D#D### 1
  #B#C#A#A#   2
  #########

  Example
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
*)

let grid = Array.make_matrix 3 11 0

(* let start_config =
     [|
       (2, 2);
       (8, 2);
       (2, 1);
       (6, 1);
       (4, 1);
       (6, 2);
       (4, 2);
       (8, 1);
     |]
   ;; *)

let start_config =
  [|
    (6, 2);
    (8, 2);
    (2, 2);
    (4, 1);
    (2, 1);
    (4, 2);
    (6, 1);
    (8, 1);
  |]
;;

let set_grid config =
  for i = 0 to 10 do
    for j = 0 to 2 do
      grid.(j).(i) <- 0
    done
  done;
  for i = 0 to 7 do
    let x, y = config.(i) in
    grid.(y).(x) <- (i / 2) + 1
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
  print_string "  ";
  process 2 1;
  print_string " ";
  process 4 1;
  print_string " ";
  process 6 1;
  print_string " ";
  process 8 1;
  print_newline ();
  print_string "  ";
  process 2 2;
  print_string " ";
  process 4 2;
  print_string " ";
  process 6 2;
  print_string " ";
  process 8 2;
  print_newline ();
  print_newline ()
;;

let stack = ref E
let costs = [| 1; 10; 100; 1000 |]

let print_states () =
  let rec aux = function
    | E -> ()
    | N { value; score; left; right } ->
        Printf.printf "Score: %d\n" score;
        print_config value;
        aux left;
        aux right
  in
  aux !stack
;;

exception Stop

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
    for i = 0 to 7 do
      let x, y = config.(i) in
      if y = 0 || x / 2 <> (i / 2) + 1 then raise Stop
    done;
    true
  with Stop -> false
;;

let move i nx ny config cost =
  let ox, oy = config.(i) in
  let new_config = Array.copy config in
  new_config.(i) <- (nx, ny);
  let ii = i / 2 * 2 in
  if new_config.(ii + 1) < new_config.(ii) then (
    (* On a des symétries dans les configurations *)
    let a = new_config.(ii) in
    new_config.(ii) <- new_config.(ii + 1);
    new_config.(ii + 1) <- a);
  let delta =
    costs.(i / 2) * (abs (nx - ox) + abs (ny - oy))
  in
  stack := insert new_config (cost + delta) !stack
;;

let h = Hashtbl.create 1000

let process_state config cost =
  set_grid config;
  for i = 0 to 7 do
    (* try to move i *)
    let x, y = config.(i)
    and t = (i / 2) + 1 in
    if
      (y = 1 && (x <> 2 * t || grid.(2).(x) <> t))
      || (y = 2 && x <> 2 * t && grid.(1).(x) = 0)
    then
      List.iter
        (fun xx ->
          if can_reach x xx then move i xx 0 config cost)
        [ 0; 1; 3; 5; 7; 9; 10 ];
    if
      (* grande salle *)
      y = 0 && can_reach x (2 * t) && grid.(1).(2 * t) = 0
    then
      if grid.(2).(2 * t) = 0 then
        move i (2 * t) 2 config cost
      else if grid.(2).(2 * t) = t then
        move i (2 * t) 1 config cost
  done
;;

let rec dijkstra () =
  match extract !stack with
  | None -> failwith "Dijkstra"
  | Some (conf, cost, stack') ->
      stack := stack';
      if not (Hashtbl.mem h conf) then begin
        Hashtbl.add h conf ();
        (* Printf.printf "Score: %d\n" cost; *)
        (* print_config conf; *)
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
  stack := insert start_config 0 E;
  Hashtbl.reset h;
  Printf.printf "%d\n" (dijkstra ())
;;
