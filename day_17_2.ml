(* Day 17 *)
(* On explore avec style... mais on programme pas vraiment avec style, mais ce n'est pas le sujet. *)

exception Stop

let _ =
  let input = Scanf.Scanning.open_in "input_17.txt" in
  let xmin, xmax, ymin, ymax =
    Scanf.bscanf input "target area: x=%d..%d, y=%d..%d\n"
      (fun a b c d -> (a, b, c, d))
  in
  Scanf.Scanning.close_in input;
  let cnt = ref 0 in
  for vx = 0 to xmax do
    for vy = ymin to -1 - ymin do
      let x = ref 0
      and y = ref 0
      and vvx = ref vx
      and vvy = ref vy in
      while
        (!x < xmin || !y > ymax) && !x <= xmax && !y >= ymin
      do
        x := !x + !vvx;
        y := !y + !vvy;
        decr vvx;
        if !vvx < 0 then vvx := 0;
        decr vvy
      done;
      if !x <= xmax && !y >= ymin then incr cnt
    done
  done;
  Printf.printf "%d" !cnt
;;
