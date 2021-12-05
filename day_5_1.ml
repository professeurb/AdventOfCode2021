(* Day 5 *)
(* On évite les sources hydrothermales. *)

module Coords = struct
  type t = int * int

  let compare ((x1, y1) : t) ((x2, y2) : t) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | v -> v
  ;;
end

module Carte = Map.Make (Coords)

type ligne =
  | H of int * int * int
  | V of int * int * int
  | O

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let process_point x y carte cnt =
  match Carte.find_opt (x, y) !carte with
  | None -> carte := Carte.add (x, y) false !carte
  | Some false ->
      incr cnt;
      carte := Carte.add (x, y) true !carte
  | _ -> ()
;;

let _ =
  let input = Scanf.Scanning.open_in "input_5.txt" in
  try
    let s =
      stream_of_file input "%d,%d -> %d,%d\n"
        (fun x1 y1 x2 y2 ->
          if x1 = x2 then
            V (x1, min y1 y2, max y1 y2)
          else if y1 = y2 then
            H (min x1 x2, max x1 x2, y1)
          else
            O)
    in
    let carte = ref Carte.empty
    and cnt = ref 0 in
    Stream.iter
      (fun l ->
        match l with
        | H (x1, x2, y) ->
            for x = x1 to x2 do
              process_point x y carte cnt
            done
        | V (x, y1, y2) ->
            for y = y1 to y2 do
              process_point x y carte cnt
            done
        | O -> ())
      s;
    Printf.printf "%d\n" !cnt
  with e ->
    Scanf.Scanning.close_in input;
    raise e
;;
