(* Day 19 *)
(* On repère des balises. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let get_number g l c =
  let v = ref 0 in
  for i = l - 1 to l + 1 do
    for j = c - 1 to c + 1 do
      v := (2 * !v) + g.(i).(j)
    done
  done;
  !v
;;

let decode_char = function
  | '.' -> 0
  | '#' -> 1
  | _ -> failwith "decode_char"
;;

let _ =
  let input = Scanf.Scanning.open_in "input_20.txt" in
  let rules =
    Scanf.bscanf input "%s\n\n" (fun x -> x)
    |> String.to_seq |> Seq.map decode_char |> Array.of_seq
  in
  let grid =
    let g = ref []
    and line_stream =
      stream_of_file input "%s\n" (fun x ->
          x |> String.to_seq |> Seq.map decode_char
          |> Array.of_seq)
    in
    (try
       while true do
         g := Stream.next line_stream :: !g
       done
     with _ -> ());
    Array.of_list (List.rev !g)
  in
  Scanf.Scanning.close_in input;
  let h = Array.length grid
  and w = Array.length grid.(0)
  and padding = 10 in
  let grid0 =
    Array.make_matrix
      (h + (2 * padding))
      (w + (2 * padding))
      0
  in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      grid0.(i + padding).(j + padding) <- grid.(i).(j)
    done
  done;
  let grid1 =
    Array.make_matrix
      (h + (2 * padding))
      (w + (2 * padding))
      rules.(0)
  in
  for k = 1 to 2 do
    for i = 1 to h + (2 * padding) - 2 do
      for j = 1 to w + (2 * padding) - 2 do
        grid1.(i).(j) <- rules.(get_number grid0 i j)
      done
    done;
    for i = 0 to h + (2 * padding) - 1 do
      grid1.(i).(0) <- grid1.(1).(1);
      grid1.(i).(w + (2 * padding) - 1) <- grid1.(1).(1)
    done;
    for j = 0 to w + (2 * padding) - 1 do
      grid1.(0).(j) <- grid1.(1).(1);
      grid1.(h + (2 * padding) - 1).(j) <- grid1.(1).(1)
    done;
    for i = 0 to h + (2 * padding) - 1 do
      for j = 0 to w + (2 * padding) - 1 do
        grid0.(i).(j) <- grid1.(i).(j)
      done
    done
  done;
  let cnt = ref 0 in
  for i = 1 to h + (2 * padding) - 2 do
    for j = 1 to w + (2 * padding) - 2 do
      cnt := !cnt + grid0.(i).(j)
    done
  done;
  Printf.printf "%d\n" !cnt
;;
