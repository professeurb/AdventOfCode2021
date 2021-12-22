(* Day 22 *)
(* On allume des cubes. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
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
  let grid =
    Array.init 101 (fun _ ->
        Array.make_matrix 101 101 false)
  in
  Stream.iter
    (fun (v, x1, x2, y1, y2, z1, z2) ->
      for x = max x1 (-50) to min x2 50 do
        for y = max y1 (-50) to min y2 50 do
          for z = max z1 (-50) to min z2 50 do
            grid.(x + 50).(y + 50).(z + 50) <- v
          done
        done
      done)
    step_stream;
  Scanf.Scanning.close_in input;
  let cnt = ref 0 in
  for x = 0 to 100 do
    for y = 0 to 100 do
      for z = 0 to 100 do
        if grid.(x).(y).(z) then incr cnt
      done
    done
  done;
  Printf.printf "%d\n" cnt
;;
