(* Day 8 *)
(* Affichage défectueux. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let rec fold_stream f a s =
  match Stream.peek s with
  | Some v ->
      Stream.junk s;
      fold_stream f (f a v) s
  | None -> a
;;

let fold_string f a s =
  let v = ref a in
  for i = 0 to String.length s - 1 do
    v := f !v s.[i]
  done;
  !v
;;

exception Chunck of int

let _ =
  let input = Scanf.Scanning.open_in "input_10.txt" in
  let s = stream_of_file input "%s\n" (fun x -> x) in
  let score = ref 0 in
  Stream.iter
    (fun line ->
      try
        ignore
          (fold_string
             (fun stack char ->
               match (char, stack) with
               | '<', _ | '(', _ | '[', _ | '{', _ ->
                   char :: stack
               | '>', '<' :: stack'
               | ')', '(' :: stack'
               | ']', '[' :: stack'
               | '}', '{' :: stack' ->
                   stack'
               | ')', _ -> raise (Chunck 3)
               | ']', _ -> raise (Chunck 57)
               | '}', _ -> raise (Chunck 1197)
               | '>', _ -> raise (Chunck 25137)
               | _ -> failwith "Illegal character")
             [] line)
      with Chunck pts -> score := !score + pts)
    s;
  (* Process last line *)
  Scanf.Scanning.close_in input;
  Printf.printf "%d\n" !score
;;
