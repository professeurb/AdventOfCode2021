(* Day 19 *)
(* On repère des balises. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let tidy p q =
  let v1 = abs (p.(0) - q.(0))
  and v2 = abs (p.(1) - q.(1))
  and v3 = abs (p.(2) - q.(2)) in
  (* assert (v1 <> v2) ;
     assert (v1 <> v3) ;
     assert (v2 <> v3) ; *)
  List.sort compare [ v1; v2; v3 ]
;;

let rec check_no_double = function
  | (a, _) :: (b, _) :: _ when a = b -> false
  | _ :: l -> check_no_double l
  | [] -> true
;;

let process_positions a =
  let deltas = ref [] in
  for i = 1 to Array.length a - 1 do
    for j = 0 to i - 1 do
      deltas := (tidy a.(i) a.(j), (j, i)) :: !deltas
    done
  done;
  let res = List.sort compare !deltas in
  assert (check_no_double res);
  res
;;

let rec correlations l1 l2 =
  match (l1, l2) with
  | (p1, _) :: l1', (p2, _) :: l2' when p1 = p2 ->
      1 + correlations l1' l2'
  | (p1, _) :: l1', (p2, _) :: l2' when p1 < p2 ->
      correlations l1' l2
  | (p1, _) :: l1', (p2, _) :: l2' when p1 > p2 ->
      correlations l1 l2'
  | _ -> 0
;;

let rec step1 l1 l2 =
  match (l1, l2) with
  | (p1, b1) :: l1', (p2, b2) :: l2'
    when p1 = p2
         && List.nth p1 0 > 0
         && List.nth p1 0 <> List.nth p1 1
         && List.nth p1 1 <> List.nth p1 2 ->
      (b1, b2, l1', l2')
  | (p1, b1) :: l1', (p2, b2) :: l2' when p1 = p2 ->
      step1 l1' l2'
  | (p1, _) :: l1', (p2, _) :: l2' when p1 < p2 ->
      step1 l1' l2
  | (p1, _) :: l1', (p2, _) :: l2' when p1 > p2 ->
      step1 l1 l2'
  | _ -> failwith "grrrrh-step1"
;;

let step2 i1 (j1, j2) l1 l2 =
  let rec aux l1 l2 =
    match (l1, l2) with
    | (p1, (b11, b12)) :: l1', (p2, (b21, b22)) :: l2'
      when p1 = p2 && (b11 = i1 || b12 = i1) ->
        if b21 = j1 then
          (j1, j2)
        else if b22 = j1 then
          (j1, j2)
        else if b21 = j2 then
          (j2, j1)
        else if b22 = j2 then
          (j2, j1)
        else
          failwith "step 2 grrrrh1"
    | (p1, _) :: l1', (p2, _) :: l2' when p1 = p2 ->
        aux l1' l2'
    | (p1, _) :: l1', (p2, _) :: l2' when p1 < p2 ->
        aux l1' l2
    | (p1, _) :: l1', (p2, _) :: l2' when p1 > p2 ->
        aux l1 l2'
    | _ -> failwith "step 2 grrrrh2"
  in
  aux l1 l2
;;

let pick_beacons l1 l2 =
  let (i1, i2), (j1, j2), l1', l2' = step1 l1 l2 in
  let j1, j2 = step2 i1 (j1, j2) l1' l2' in
  ((i1, i2), (j1, j2))
;;

let swapxy a =
  for i = 0 to Array.length a - 1 do
    let tmp = a.(i).(0) in
    a.(i).(0) <- a.(i).(1);
    a.(i).(1) <- tmp
  done
;;

let swapxz a =
  for i = 0 to Array.length a - 1 do
    let tmp = a.(i).(0) in
    a.(i).(0) <- a.(i).(2);
    a.(i).(2) <- tmp
  done
;;

let swapyz a =
  for i = 0 to Array.length a - 1 do
    let tmp = a.(i).(1) in
    a.(i).(1) <- a.(i).(2);
    a.(i).(2) <- tmp
  done
;;

let apply_dirs dx dy dz a =
  for i = 0 to Array.length a - 1 do
    a.(i).(0) <- a.(i).(0) * dx;
    a.(i).(1) <- a.(i).(1) * dy;
    a.(i).(2) <- a.(i).(2) * dz
  done
;;

let apply_delta dx dy dz a =
  for i = 0 to Array.length a - 1 do
    a.(i).(0) <- a.(i).(0) + dx;
    a.(i).(1) <- a.(i).(1) + dy;
    a.(i).(2) <- a.(i).(2) + dz
  done
;;

let connect sc prsc i j =
  let (i1, i2), (j1, j2) = pick_beacons prsc.(i) prsc.(j) in
  (* Order of coordinates *)
  if
    abs (sc.(i).(i1).(0) - sc.(i).(i2).(0))
    = abs (sc.(j).(j1).(1) - sc.(j).(j2).(1))
  then
    swapxy sc.(j)
  else if
    abs (sc.(i).(i1).(0) - sc.(i).(i2).(0))
    = abs (sc.(j).(j1).(2) - sc.(j).(j2).(2))
  then
    swapxz sc.(j);
  if
    abs (sc.(i).(i1).(1) - sc.(i).(i2).(1))
    = abs (sc.(j).(j1).(2) - sc.(j).(j2).(2))
  then
    swapyz sc.(j);
  apply_dirs
    ((sc.(i).(i1).(0) - sc.(i).(i2).(0))
    / (sc.(j).(j1).(0) - sc.(j).(j2).(0)))
    ((sc.(i).(i1).(1) - sc.(i).(i2).(1))
    / (sc.(j).(j1).(1) - sc.(j).(j2).(1)))
    ((sc.(i).(i1).(2) - sc.(i).(i2).(2))
    / (sc.(j).(j1).(2) - sc.(j).(j2).(2)))
    sc.(j);
  apply_delta
    (sc.(i).(i1).(0) - sc.(j).(j1).(0))
    (sc.(i).(i1).(1) - sc.(j).(j1).(1))
    (sc.(i).(i1).(2) - sc.(j).(j1).(2))
    sc.(j);
  assert (sc.(i).(i2).(0) = sc.(j).(j2).(0));
  assert (sc.(i).(i2).(1) = sc.(j).(j2).(1));
  assert (sc.(i).(i2).(2) = sc.(j).(j2).(2))
;;

let _ =
  let input = Scanf.Scanning.open_in "input_19.txt" in
  let beacon_stream =
    stream_of_file input "%d,%d,%d\n" (fun x y z ->
        [| x; y; z |])
  in
  let next_scanner () =
    let scanner =
      Scanf.bscanf input "--- scanner %d ---\n" (fun x -> x)
    and beacons = ref [] in
    (try
       Stream.iter
         (fun b -> beacons := b :: !beacons)
         beacon_stream
     with _ -> ());
    Scanf.bscanf input "\n" (fun () -> ()) ();
    (scanner, Array.of_list !beacons)
  in
  let prescanners = ref [] in
  (try
     while true do
       prescanners := next_scanner () :: !prescanners
     done
   with _ -> ());
  Scanf.Scanning.close_in input;
  let scanners =
    Array.init (List.length !prescanners) (fun i ->
        List.assoc i !prescanners)
  in
  let processed_scanners =
    Array.map process_positions scanners
  in
  let nb_scanners = Array.length scanners in
  let todo = Array.make nb_scanners true in
  todo.(0) <- false;
  let rec visit s =
    (* Printf.printf "Visiting %d\n" s; *)
    for i = 0 to nb_scanners - 1 do
      if
        todo.(i)
        && correlations
             processed_scanners.(s)
             processed_scanners.(i)
           = 66
      then begin
        todo.(i) <- false;
        connect scanners processed_scanners s i;
        visit i
      end
    done
    (* Printf.printf "Done %d\n" s *)
  in
  visit 0;

  let b_list = ref [] in
  Array.iter
    (fun beacons ->
      Array.iter
        (fun beacon ->
          if not (List.mem beacon !b_list) then
            b_list := beacon :: !b_list)
        beacons)
    scanners;
  Printf.printf "Part One : %d\n" (List.length !b_list)
;;
