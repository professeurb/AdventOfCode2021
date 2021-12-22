(* Day 22 *)
(* On allume des cubes. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

(* let pos v a =
     let rec aux p = if a.(p) = v then p else aux (p + 1) in
     aux 0
   ;; *)

(* On fait une dichotomie, quand même. *)
let pos v a =
  let deb = ref 0
  and fin = ref (Array.length a) in
  (* k < deb => a.(k) < v *)
  (* k >= deb => a.(k) >= v *)
  while !deb < !fin do
    let mil = (!deb + !fin) / 2 in
    if a.(mil) >= v then fin := mil else deb := mil + 1
  done;
  !deb
;;

let _ =
  let input = Scanf.Scanning.open_in "input_22.txt" in
  let step_stream =
    stream_of_file input "%s x=%d..%d,y=%d..%d,z=%d..%d\n"
      (fun o x1 x2 y1 y2 z1 z2 ->
        ( (match o with
          | "on" -> true
          | "off" -> false
          | _ -> failwith "unexpected order"),
          x1,
          x2,
          y1,
          y2,
          z1,
          z2 ))
  in
  let instructions = ref [] in
  Stream.iter
    (fun i -> instructions := i :: !instructions)
    step_stream;
  Scanf.Scanning.close_in input;
  let xs = ref []
  and ys = ref []
  and zs = ref [] in
  List.iter
    (fun (_, x1, x2, y1, y2, z1, z2) ->
      xs := x1 :: (x2 + 1) :: !xs)
    !instructions;
  let cnt = ref 0
  and xs = Array.of_list (List.sort_uniq compare !xs) in
  for i = 0 to Array.length xs - 2 do
    ys := [];
    zs := [];
    let instr =
      List.fold_left
        (fun l (v, x1, x2, y1, y2, z1, z2) ->
          if x1 <= xs.(i) && x2 >= xs.(i + 1) - 1 then (
            ys := y1 :: (y2 + 1) :: !ys;
            zs := z1 :: (z2 + 1) :: !zs;
            (v, y1, y2, z1, z2) :: l)
          else
            l)
        [] !instructions
    in
    let ys = Array.of_list (List.sort_uniq compare !ys)
    and zs = Array.of_list (List.sort_uniq compare !zs) in
    let grid =
      Array.make_matrix (Array.length ys) (Array.length zs)
        false
    in
    List.iter
      (fun (v, y1, y2, z1, z2) ->
        for j = pos y1 ys to pos (y2 + 1) ys - 1 do
          for k = pos z1 zs to pos (z2 + 1) zs - 1 do
            grid.(j).(k) <- v
          done
        done)
      instr;
    for j = 0 to Array.length ys - 2 do
      for k = 0 to Array.length zs - 2 do
        if grid.(j).(k) then
          cnt :=
            !cnt
            + (xs.(i + 1) - xs.(i))
              * (ys.(j + 1) - ys.(j))
              * (zs.(k + 1) - zs.(k))
      done
    done
  done;
  Printf.printf "%d\n" !cnt
;;
