(* Day 11 *)
(* La danse des poulpes. *)

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

let _ =
  let input = Scanf.Scanning.open_in "input_11.txt" in
  let s =
    stream_of_file input "%s\n" (fun s ->
        s |> String.to_seq
        |> Seq.map (fun c -> Char.code c - 48)
        |> Array.of_seq)
  in
  let lines = ref [] in
  Stream.iter (fun line -> lines := line :: !lines) s;
  let grid = Array.of_list !lines in
  let flashes = ref 0
  and height = Array.length grid
  and width = Array.length grid.(0)
  and flashlist = ref [] in
  let rec powerup l c =
    if l >= 0 && l < height && c >= 0 && c < width then begin
      grid.(l).(c) <- grid.(l).(c) + 1;
      if grid.(l).(c) = 10 then begin
        incr flashes;
        flashlist := (l, c) :: !flashlist;
        powerup (l - 1) (c - 1);
        powerup (l - 1) c;
        powerup (l - 1) (c + 1);
        powerup l (c - 1);
        powerup l (c + 1);
        powerup (l + 1) (c - 1);
        powerup (l + 1) c;
        powerup (l + 1) (c + 1)
      end
    end
  in
  for step = 1 to 100 do
    flashlist := [];
    for l = 0 to height - 1 do
      for c = 0 to width - 1 do
        powerup l c
      done
    done;
    List.iter (fun (l, c) -> grid.(l).(c) <- 0) !flashlist
  done;
  (* Process last line *)
  Scanf.Scanning.close_in input;
  Printf.printf "%d\n" !flashes
;;
