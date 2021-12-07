(* Day 7 *)
(* On fait de la trichotomie avec des crabes. *)

let id x = x

let cost n = n * (n + 1) / 2

let score crabs pos =
  let s = ref 0 in
  for i = 0 to Array.length crabs - 1 do
    s := cost (abs (crabs.(i) - pos)) + !s
  done;
  !s
;;

let _ =
  let input = Scanf.Scanning.open_in "input_7.txt" in
  try
    let pre_crabs = ref [] in
    let d = Scanf.bscanf input "%d" id in
    pre_crabs := d :: !pre_crabs;
    (try
       while true do
         let d = Scanf.bscanf input ",%d" id in
         pre_crabs := d :: !pre_crabs
       done
     with Stdlib.Scanf.Scan_failure _ -> ());
    Scanf.Scanning.close_in input;
    let crabs = Array.of_list !pre_crabs in
    Array.sort compare crabs;
    let deb = ref 0
    and fin = ref (Array.length crabs - 1) in
    let score_deb = ref (score crabs crabs.(!deb))
    and score_fin = ref (score crabs crabs.(!fin)) in
    while !fin >= !deb + 3 do
      let ecart = (!fin - !deb) / 3 in
      let d1 = !deb + ecart
      and d2 = !fin - ecart in
      let score_d1 = score crabs crabs.(d1)
      and score_d2 = score crabs crabs.(d2) in
      if score_d1 >= !score_fin then (
        deb := d1;
        score_deb := score_d1)
      else if score_d2 >= !score_deb then (
        fin := d2;
        score_fin := score_d2)
      else (
        deb := d1;
        score_deb := score_d1;
        fin := d2;
        score_fin := score_d2)
    done;
    let score_final = ref !score_deb in
    for p = crabs.(!deb) + 1 to crabs.(!fin) do
      let s = score crabs p in
      if s < !score_final then score_final := s
    done;
    Printf.printf "%d\n" !score_final
  with e ->
    Scanf.Scanning.close_in input;
    raise e
;;
