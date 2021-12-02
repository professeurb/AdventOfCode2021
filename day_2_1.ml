(* Day 2 *)
(* Aujourd'hui, on va travailler un peu l'import de données.
   On utilise la bibliothèque Scanf.
   De plus, en cas de problème, on ferme le fichier
   avant propagation de l'exception.
*)

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

let _ =
  let input = Scanf.Scanning.open_in "input_2.txt" in
  try
    let s =
      stream_of_file input "%s %d\n" (fun a b -> (a, b))
    in
    let x = ref 0
    and y = ref 0 in
    Stream.iter
      (fun (dir, amt) ->
        match dir with
        | "forward" -> x := !x + amt
        | "up" -> y := !y - amt
        | "down" -> y := !y + amt
        | _ -> failwith "Unknown direction")
      s;
    Printf.printf "%d\n" (!x * !y);
    Scanf.Scanning.close_in input
  with _ -> Scanf.Scanning.close_in input
;;
