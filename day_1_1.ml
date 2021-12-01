(* Day 1 *)
(* C'est facile, et c'est l'occasion de mettre en place
   des fonctions utilitaires. *)

let stream_of_file f =
  let input = open_in f in
  let streamer _ =
    try Some (input_line input)
    with End_of_file ->
      close_in_noerr input;
      None
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
  let s1 = stream_of_file "input_1.txt" in
  let s2 = map_stream int_of_string s1 in
  let cnt = ref 0
  and cur = ref (Stream.next s2) in
  Stream.iter
    (fun v ->
      if v > !cur then incr cnt;
      cur := v)
    s2;
  Printf.printf "%d\n" !cnt
;;
