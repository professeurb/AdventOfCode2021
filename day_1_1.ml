(* Day 1 *)
(* C'est facile, et c'est l'occasion de mettre en place
   des fonctions utilitaires. *)

let process_file f p =
  let input = open_in f in
  try
    let streamer _ =
      try Some (input_line input)
      with End_of_file ->
        close_in_noerr input;
        None
    in
    p (Stream.from streamer)
  with e ->
    close_in_noerr input;
    raise e
;;

let map_stream f s =
  Stream.from (fun _ ->
      match Stream.peek s with
      | Some v ->
          Stream.junk s;
          Some (f v)
      | None -> None)
;;

let day_1_1 s =
  let s' = map_stream int_of_string s in
  let cnt = ref 0
  and cur = ref (Stream.next s') in
  Stream.iter
    (fun v ->
      if v > !cur then incr cnt;
      cur := v)
    s';
  !cnt
;;

let _ =
  print_int (process_file "input_1.txt" day_1_1);
  print_newline ()
;;
