(* Day 15 *)
(* On cherche un chemin. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

(* Méthode sale *)

let _ =
  let input = Scanf.Scanning.open_in "input_15.txt" in
  (* Read starting template *)
  let line_stream =
    stream_of_file input "%s\n" (fun s ->
        s |> String.to_seq
        |> Seq.map (fun c -> Char.code c - 48)
        |> Array.of_seq)
  in
  let lines = ref [] in
  Stream.iter
    (fun line -> lines := line :: !lines)
    line_stream;
  Scanf.Scanning.close_in input;
  let grid = Array.of_list (List.rev !lines) in
  let h0 = Array.length grid
  and w0 = Array.length grid.(0) in
  let h = 5 * h0
  and w = 5 * w0 in
  let grid_score l c =
    let delta = (l / h0) + (c / w0) in
    1 + ((grid.(l mod h0).(c mod w0) + delta - 1) mod 9)
  in
  let score = Array.make_matrix h w (max_int / 2) in
  score.(0).(0) <- 0;
  let continue = ref true
  and curr = ref 0 in
  (* Oui, vous lisez bien, on itère tant que
     ce n'est pas stabilisé. *)
  while !continue do
    continue := false;
    for l = 0 to h - 1 do
      for c = 0 to w - 1 do
        curr := score.(l).(c);
        if l > 0 then curr := min !curr score.(l - 1).(c);
        if l < h - 1 then
          curr := min !curr score.(l + 1).(c);
        if c > 0 then curr := min !curr score.(l).(c - 1);
        if c < w - 1 then
          curr := min !curr score.(l).(c + 1);
        let new_score = !curr + grid_score l c in
        if new_score < score.(l).(c) then begin
          continue := true;
          score.(l).(c) <- new_score
        end
      done
    done
  done;
  Printf.printf "%d\n" score.(h - 1).(w - 1)
;;
