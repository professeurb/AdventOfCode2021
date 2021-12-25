(* Day 25 *)
(* On attends les concombres de mer. *)

type case = D | B | V

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let convert_char = function
  | '>' -> D
  | 'v' -> B
  | '.' -> V
  | _ -> failwith "Bad character"
;;

let step_right grid =
  let moved = ref false in
  let w = Array.length grid.(0) in
  for l = 0 to Array.length grid - 1 do
    let p = ref 0 in
    while grid.(l).(!p) = D do
      incr p
    done;
    let c = ref !p in
    while !c < !p + w do
      if
        grid.(l).(!c mod w) = D
        && grid.(l).((!c + 1) mod w) = V
      then begin
        grid.(l).(!c mod w) <- V;
        grid.(l).((!c + 1) mod w) <- D;
        moved := true;
        incr c
      end;
      incr c
    done
  done;
  !moved
;;

let step_down grid =
  let moved = ref false in
  let h = Array.length grid in
  for c = 0 to Array.length grid.(0) - 1 do
    let p = ref 0 in
    while grid.(!p).(c) = B do
      incr p
    done;
    let l = ref !p in
    while !l < !p + h do
      if
        grid.(!l mod h).(c) = B
        && grid.((!l + 1) mod h).(c) = V
      then begin
        grid.(!l mod h).(c) <- V;
        grid.((!l + 1) mod h).(c) <- B;
        moved := true;
        incr l
      end;
      incr l
    done
  done;
  !moved
;;

let step grid =
  let a = step_right grid
  and b = step_down grid in
  a || b
;;

let _ =
  let input = open_in "input_25.txt" in
  let pre_grid = ref [] in
  (try
     while true do
       let line = input_line input in
       pre_grid :=
         (line |> String.to_seq
         |> Seq.map convert_char
         |> Array.of_seq)
         :: !pre_grid
     done
   with End_of_file -> ());
  close_in input;
  let grid = Array.of_list (List.rev !pre_grid) in
  let cnt = ref 0 in
  while step grid do
    incr cnt
  done;
  incr cnt;
  Printf.printf "%d\n" !cnt
;;
