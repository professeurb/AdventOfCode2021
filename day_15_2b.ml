(* Day 15 *)
(* On cherche un chemin. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

(* Méthode avec Dijkstra *)

(* Leftist heaps *)

type 'a abr =
  | E
  | N of {
      value : 'a;
      score : int;
      rank : int;
      left : 'a abr;
      right : 'a abr;
    }

let rank = function
  | E -> 0
  | N { rank } -> rank
;;

let make x s a b =
  if rank a >= rank b then
    N
      {
        value = x;
        score = s;
        rank = 1 + rank b;
        left = a;
        right = b;
      }
  else
    N
      {
        value = x;
        score = s;
        rank = 1 + rank a;
        left = b;
        right = a;
      }
;;

let rec merge t1 t2 =
  match (t1, t2) with
  | t, E | E, t -> t
  | N s1, N s2 ->
      if s1.score <= s2.score then
        make s1.value s1.score s1.left (merge s1.right t2)
      else
        make s2.value s2.score s2.left (merge t1 s2.right)
;;

let insert v s t =
  merge
    (N
       {
         value = v;
         score = s;
         rank = 1;
         left = E;
         right = E;
       })
    t
;;

let extract = function
  | E -> None
  | N s -> Some (s.value, s.score, merge s.left s.right)
;;

exception Stop

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
  let score = Array.make_matrix h w max_int
  and heap = ref (insert (0, 0) 0 E) in
  try
    while true do
      match extract !heap with
      | None -> failwith "empty heap"
      | Some ((l, c), s, he) ->
          heap := he;
          if s < score.(l).(c) then begin
            score.(l).(c) <- s;
            if l = h - 1 && c = w - 1 then raise Stop;
            if
              l > 0
              && s + grid_score (l - 1) c
                 < score.(l - 1).(c)
            then
              heap :=
                insert
                  (l - 1, c)
                  (s + grid_score (l - 1) c)
                  !heap;
            if
              l < h - 1
              && s + grid_score (l + 1) c
                 < score.(l + 1).(c)
            then
              heap :=
                insert
                  (l + 1, c)
                  (s + grid_score (l + 1) c)
                  !heap;
            if
              c > 0
              && s + grid_score l (c - 1)
                 < score.(l).(c - 1)
            then
              heap :=
                insert
                  (l, c - 1)
                  (s + grid_score l (c - 1))
                  !heap;
            if
              c < w - 1
              && s + grid_score l (c + 1)
                 < score.(l).(c + 1)
            then
              heap :=
                insert
                  (l, c + 1)
                  (s + grid_score l (c + 1))
                  !heap
          end
    done
  with Stop -> Printf.printf "%d\n" score.(h - 1).(w - 1)
;;
