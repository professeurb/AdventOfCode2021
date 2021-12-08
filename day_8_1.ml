(* Day 8 *)
(* Affichage défectueux. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let _ =
  let input = Scanf.Scanning.open_in "input_8.txt" in
  let s =
    stream_of_file input
      "%s %s %s %s %s %s %s %s %s %s | %s %s %s %s\n"
      (fun a b c d e f g h i j d1 d2 d3 d4 ->
        ( [| a; b; c; d; e; f; g; h; i; j |],
          [ d1; d2; d3; d4 ] ))
  in
  let cnt = ref 0 in
  Stream.iter
    (fun (_, right) ->
      List.iter
        (fun d ->
          match String.length d with
          | 2 | 3 | 4 | 7 -> incr cnt
          | _ -> ())
        right)
    s;
  Scanf.Scanning.close_in input;
  Printf.printf "%d\n" !cnt
;;
