(* Day 2 *)
(* On ajoute un nouvel itérateur de streams. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let map_stream f s =
  Stream.from (fun _ ->
      match Stream.peek s with
      | Some v ->
          Stream.junk s;
          Some (f v)
      | None -> None)
;;

let rec fold_stream f a s =
  match Stream.peek s with
  | Some v ->
      Stream.junk s;
      fold_stream f (f a v) s
  | None -> a
;;

let _ =
  let input = Scanf.Scanning.open_in "input_2.txt" in
  try
    let s =
      stream_of_file input "%s %d\n" (fun a b -> (a, b))
    in
    let x, y, _ =
      fold_stream
        (fun (xpos, depth, aim) (dir, amount) ->
          match dir with
          | "forward" ->
              (xpos + amount, depth + (aim * amount), aim)
          | "down" -> (xpos, depth, aim + amount)
          | "up" -> (xpos, depth, aim - amount)
          | _ -> failwith "Unknown direction")
        (0, 0, 0) s
    in
    Printf.printf "%d\n" (x * y);
    Scanf.Scanning.close_in input
  with _ -> Scanf.Scanning.close_in input
;;
