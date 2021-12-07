(* Day 7 *)
(* On aligne des crabes de façon convexe. *)

let id x = x

let _ =
  let input = Scanf.Scanning.open_in "input_7.txt" in
  try
    let pre_crabs = ref []
    and tot = ref 0
    and sum = ref 0 in
    let d = Scanf.bscanf input "%d" id in
    pre_crabs := d :: !pre_crabs;
    incr tot;
    sum := d + !sum;
    (try
       while true do
         let d = Scanf.bscanf input ",%d" id in
         pre_crabs := d :: !pre_crabs;
         incr tot;
         sum := d + !sum
       done
     with Stdlib.Scanf.Scan_failure _ -> ());
    Scanf.Scanning.close_in input;
    let crabs = Array.of_list !pre_crabs in
    Array.sort compare crabs;
    let pos = crabs.(((!tot + 1) / 2) - 1)
    and score = ref 0 in
    for i = 0 to !tot - 1 do
      score := !score + abs (crabs.(i) - pos)
    done;
    Printf.printf "%d\n" !score
  with e ->
    Scanf.Scanning.close_in input;
    raise e
;;
