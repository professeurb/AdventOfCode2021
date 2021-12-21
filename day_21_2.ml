(* Day 20 *)
(* On joue au dé. *)

let move p = if p <= 0 then p + 10 else p

(* Dynamic programming *)
let rec play h pos score round =
  (* We score every third die *)
  let prev_score =
    if round mod 3 = 0 then score - pos else score
  in
  if round < 0 || score < 0 || prev_score >= 21 then
    0
  else
    match Hashtbl.find_opt h (pos, score, round) with
    | Some cnt -> cnt
    | None ->
        let cnt =
          play h (move (pos - 1)) prev_score (round - 1)
          + play h (move (pos - 2)) prev_score (round - 1)
          + play h (move (pos - 3)) prev_score (round - 1)
        in
        Hashtbl.add h (pos, score, round) cnt;
        cnt
;;

let init p =
  let h = Hashtbl.create 1000 in
  Hashtbl.add h (p, 0, 0) 1;
  h
;;

let _ =
  let h1 = init 4
  and h2 = init 9 in
  let a1 = Array.make 24 0
  and a2 = Array.make 24 0 in
  (* Compute wins at round i *)
  for round = 0 to 23 do
    for pos = 1 to 10 do
      for score = 21 to 30 do
        a1.(round) <-
          a1.(round) + play h1 pos score (3 * round);
        a2.(round) <-
          a2.(round) + play h2 pos score (3 * round)
      done
    done
  done;
  (* Games not yet won at step i *)
  let b1 = Array.copy a1
  and b2 = Array.copy a2 in
  for i = 22 downto 0 do
    b1.(i) <- b1.(i) + (b1.(i + 1) / 27);
    b2.(i) <- b2.(i) + (b2.(i + 1) / 27)
  done;
  (* Count wins, at last *)
  let w1 = ref 0
  and w2 = ref 0 in
  for r1 = 1 to 23 do
    w1 := !w1 + (a1.(r1) * (b2.(r1 - 1) - a2.(r1 - 1)))
  done;
  for r2 = 0 to 23 do
    w2 := !w2 + (a2.(r2) * (b1.(r2) - a1.(r2)))
  done;
  Printf.printf "W1: %d, W2: %d\n" !w1 !w2
;;
