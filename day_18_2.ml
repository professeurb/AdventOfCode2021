(* Day 18 *)
(* On fait de l'arithmétique sous-marine. *)

type token = OB | CB | C | L of int

let is_digit c =
  Char.code c >= Char.code '0'
  && Char.code c <= Char.code '9'
;;

let readstring s =
  let tl = ref []
  and v = ref 0 in
  let pos = ref 0 in
  while !pos < String.length s do
    match s.[!pos] with
    | '[' ->
        tl := OB :: !tl;
        incr pos
    | ',' ->
        tl := C :: !tl;
        incr pos
    | ']' ->
        tl := CB :: !tl;
        incr pos
    | '0' .. '9' ->
        v := 0;
        while !pos < String.length s && is_digit s.[!pos] do
          v :=
            (10 * !v) + Char.code s.[!pos] - Char.code '0';
          incr pos
        done;
        tl := L !v :: !tl
    | _ -> failwith "Unknown character"
  done;
  List.rev !tl
;;

let rec addright v = function
  | [] -> []
  | L v' :: l -> L (v + v') :: l
  | c :: l -> c :: addright v l
;;

let explode tl =
  let rec aux depth = function
    | OB :: L lv :: C :: L rv :: CB :: l when depth >= 4 ->
        Some (lv, L 0 :: addright rv l)
    | L v :: l -> (
        match aux depth l with
        | None -> None
        | Some (lv, l') -> Some (0, L (v + lv) :: l'))
    | t :: l -> (
        let ndepth =
          match t with
          | OB -> depth + 1
          | CB -> depth - 1
          | _ -> depth
        in
        match aux ndepth l with
        | None -> None
        | Some (lv, l') -> Some (lv, t :: l'))
    | [] -> None
  in
  match aux 0 tl with
  | None -> None
  | Some (_, tl') -> Some tl'
;;

let rec split = function
  | L v :: l when v >= 10 ->
      Some
        (OB :: L (v / 2) :: C :: L (v - (v / 2)) :: CB :: l)
  | t :: l -> (
      match split l with
      | None -> None
      | Some l' -> Some (t :: l'))
  | [] -> None
;;

let rec reduce tl =
  match explode tl with
  | Some tl' -> reduce tl'
  | None -> (
      match split tl with
      | Some tl' -> reduce tl'
      | None -> tl)
;;

let magnitude tl =
  let rec aux m r = function
    | [] -> r
    | OB :: tl -> aux (3 * m) r tl
    | L v :: tl -> aux m (r + (m * v)) tl
    | C :: tl -> aux (2 * m / 3) r tl
    | CB :: tl -> aux (m / 2) r tl
  in
  aux 1 0 tl
;;

(* Je sais, j'ai honte. *)
let add tl1 tl2 = reduce (OB :: (tl1 @ (C :: tl2))) @ [ CB ]

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let _ =
  let input = Scanf.Scanning.open_in "input_18.txt" in
  let snailstream =
    stream_of_file input "%s\n" readstring
  in
  let snails = ref [] in
  Stream.iter
    (fun tl -> snails := tl :: !snails)
    snailstream;
  Scanf.Scanning.close_in input;
  let snails = Array.of_list (List.rev !snails) in
  let maxi = ref 0 in
  for i = 1 to Array.length snails - 1 do
    for j = 0 to i - 1 do
      let m = magnitude (add snails.(i) snails.(j)) in
      if m > !maxi then
        maxi := m;
      let m = magnitude (add snails.(j) snails.(i)) in
      if m > !maxi then
        maxi := m
    done
  done;
  Printf.printf "%d\n" !maxi
;;
