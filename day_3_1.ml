(* Day 3 *)
(* On manipule de la représentation binaire. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let count_ones s t =
  for i = 0 to String.length s - 1 do
    if s.[i] = '1' then t.(i) <- t.(i) + 1
  done
;;

let _ =
  let input = Scanf.Scanning.open_in "input_3.txt" in
  try
    let s = stream_of_file input "%s\n" (fun a -> a) in
    (* Count all ones and lines *)
    let cnt = ref 1
    and first = Stream.next s in
    let tab = Array.make (String.length first) 0 in
    count_ones first tab;
    Stream.iter
      (fun s ->
        incr cnt;
        count_ones s tab)
      s;
    (* Compute gamma and epsilon *)
    let gamma = ref 0
    and epsilon = ref 0 in
    for i = 0 to Array.length tab - 1 do
      gamma := !gamma * 2;
      epsilon := !epsilon * 2;
      if tab.(i) * 2 > !cnt then
        incr gamma
      else
        incr epsilon
    done;
    Printf.printf "%d\n" (!gamma * !epsilon)
  with e ->
    Scanf.Scanning.close_in input;
    raise e
;;
