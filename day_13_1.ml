(* Day 13 *)
(* On plie. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let hash_size h =
  let cnt = ref 0 in
  Hashtbl.iter (fun _ _ -> incr cnt) h;
  !cnt
;;

let fold_along h d v =
  let h2 = Hashtbl.create 100 in
  Hashtbl.iter
    (match d with
    | "x" ->
        fun (x, y) _ ->
          Hashtbl.replace h2
            (if x <= v then
               (x, y)
            else
              ((2 * v) - x, y))
            ()
    | "y" ->
        fun (x, y) _ ->
          Hashtbl.replace h2
            (if y <= v then
               (x, y)
            else
              (x, (2 * v) - y))
            ()
    | _ -> failwith "Unknown direction")
    h;
  h2
;;

let _ =
  let input = Scanf.Scanning.open_in "input_13.txt" in
  (* Read coordinates *)
  let s1 =
    (* @ is a symbol telling where to stop reading a string *)
    stream_of_file input "%d,%d\n" (fun x y -> (x, y))
  in
  let h = Hashtbl.create 100 in
  (try Stream.iter (fun p -> Hashtbl.add h p ()) s1
   with _ -> ());
  (* Pass newline *)
  Scanf.bscanf input "\n" (fun () -> ()) ();
  (* Read folding instructions *)
  let s2 =
    (* @ is a symbol telling where to stop reading a string *)
    stream_of_file input "fold along %s@=%d\n" (fun x y ->
        (x, y))
  in
  let d, v = Stream.next s2 in
  Scanf.Scanning.close_in input;
  Printf.printf "%d\n" (hash_size (fold_along h d v))
;;
