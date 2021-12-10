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
  let line = Stream.next s in
  let lines = ref [ line ]
  and width = String.length line
  and height = ref 1 in
  Stream.iter
    (fun line ->
      incr height;
      lines := line :: !lines)
    s;
  let size = !height * width in
  (* Union-Find *)
  (* On fusionne tout ce qui n'est pas 9. *)
  let uf = Array.make size (-1) in
  let rec find i =
    let r = uf.(i) in
    if r < 0 then
      i
    else
      let root = find r in
      uf.(i) <- root;
      root
  in
  let union i j =
    let ri = find i
    and rj = find j in
    if ri <> rj then
      let si = uf.(ri)
      and sj = uf.(rj) in
      if si < sj then (
        uf.(rj) <- si + sj;
        uf.(ri) <- rj)
      else (
        uf.(ri) <- si + sj;
        uf.(rj) <- ri)
  in
  let line_number = ref 0 in
  (try
     while true do
       match !lines with
       | [] -> failwith "error"
       | [ l ] ->
           for i = 0 to width - 2 do
             if l.[i] <> '9' && l.[i + 1] <> '9' then
               union
                 ((!line_number * width) + i)
                 ((!line_number * width) + i + 1)
           done;
           lines := []
       | l1 :: l2 :: lines' ->
           for i = 0 to width - 2 do
             if l1.[i] <> '9' && l1.[i + 1] <> '9' then
               union
                 ((!line_number * width) + i)
                 ((!line_number * width) + i + 1)
           done;
           for i = 0 to width - 1 do
             if l1.[i] <> '9' && l2.[i] <> '9' then
               union
                 ((!line_number * width) + i)
                 ((!line_number * width) + i + width)
           done;
           lines := l2 :: lines';
           incr line_number
     done
   with _ -> ());
  Array.sort compare uf;
  Scanf.Scanning.close_in input;
  Printf.printf "%d\n" (-uf.(0) * uf.(1) * uf.(2))
;;
