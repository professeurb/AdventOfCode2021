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

let _ =
  let input = Scanf.Scanning.open_in "input_9.txt" in
  let s = stream_of_file input "%s\n" (fun x -> x) in
  let s1 = Stream.next s
  and s2 = Stream.next s
  and cnt = ref 0 in
  let n = String.length s1 in
  (* Process first line *)
  if s1.[0] < s1.[1] && s1.[0] < s2.[0] then
    cnt := !cnt + Char.code s1.[0] - 47;
  for i = 1 to n - 2 do
    if
      s1.[i] < s1.[i - 1]
      && s1.[i] < s1.[i + 1]
      && s1.[i] < s2.[i]
    then
      cnt := !cnt + Char.code s1.[i] - 47
  done;
  if s1.[n - 1] < s1.[n - 2] && s1.[n - 1] < s2.[n - 1] then
    cnt := !cnt + Char.code s1.[n - 1] - 47;
  (* Process next and not last lines *)
  let s0, s1 =
    fold_stream
      (fun (s0, s1) s2 ->
        if
          s1.[0] < s1.[1]
          && s1.[0] < s0.[0]
          && s1.[0] < s2.[0]
        then
          cnt := !cnt + Char.code s1.[0] - 47;
        for i = 1 to n - 2 do
          if
            s1.[i] < s1.[i - 1]
            && s1.[i] < s1.[i + 1]
            && s1.[i] < s0.[i]
            && s1.[i] < s2.[i]
          then
            cnt := !cnt + Char.code s1.[i] - 47
        done;
        if
          s1.[n - 1] < s1.[n - 2]
          && s1.[n - 1] < s0.[n - 1]
          && s1.[n - 1] < s2.[n - 1]
        then
          cnt := !cnt + Char.code s1.[n - 1] - 47;
        (s1, s2))
      (s1, s2) s
  in
  if s1.[0] < s1.[1] && s1.[0] < s0.[0] then
    cnt := !cnt + Char.code s1.[0] - 47;
  for i = 1 to n - 2 do
    if
      s1.[i] < s1.[i - 1]
      && s1.[i] < s1.[i + 1]
      && s1.[i] < s0.[i]
    then
      cnt := !cnt + Char.code s1.[i] - 47
  done;
  if s1.[n - 1] < s1.[n - 2] && s1.[n - 1] < s0.[n - 1] then
    cnt := !cnt + Char.code s1.[n - 1] - 47;
  (* Process last line *)
  Scanf.Scanning.close_in input;
  Printf.printf "%d\n" !cnt
;;
