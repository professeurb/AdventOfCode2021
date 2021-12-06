(* Day 6 *)
(* On étudie les poissons-lanterne. *)

let id x = x

let _ =
  let input = Scanf.Scanning.open_in "input_6.txt" in
  try
    let fishs = Array.make 9 0 in
    let d = Scanf.bscanf input "%d" id in
    fishs.(d) <- fishs.(d) + 1;
    (try
       while true do
         let d = Scanf.bscanf input ",%d" id in
         fishs.(d) <- fishs.(d) + 1
       done
     with Stdlib.Scanf.Scan_failure _ -> ());
    Scanf.Scanning.close_in input;
    for i = 1 to 80 do
      let d0 = fishs.(0) in
      for j = 1 to 8 do
        fishs.(j - 1) <- fishs.(j)
      done;
      fishs.(8) <- d0;
      fishs.(6) <- fishs.(6) + d0
    done ;
    Printf.printf "%d\n" (Array.fold_left (fun a b -> a + b) 0 fishs)
  with e ->
    Scanf.Scanning.close_in input;
    raise e
;;
