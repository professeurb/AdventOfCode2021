(* Day 20 *)
(* On joue au dé. *)
let dice = ref 0
and roll_cnt = ref 0

let roll () =
  incr dice;
  incr roll_cnt;
  if !dice > 100 then dice := 1;
  !dice
;;

let rec play_aux p1 s1 p2 s2 =
  let advance = roll () + roll () + roll () in
  let p1' = ((p1 + advance - 1) mod 10) + 1 in
  let s1' = s1 + p1' in
  if s1' >= 1000 then
    s2 * !roll_cnt
  else
    play_aux p2 s2 p1' s1'
;;

let play p1 p2 =
  dice := 0;
  roll_cnt := 0;
  play_aux p1 0 p2 0
;;

let _ = Printf.printf "%d\n" (play 4 9)
