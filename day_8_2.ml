(* Day 8 *)
(* Affichage défectueux. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let ordered_list_of_chars s =
  List.sort compare (List.of_seq (String.to_seq s))
;;

(* Nombre d'éléments en commun entre deux
   listes croissantes.s *)
let rec intersection l1 l2 =
  match (l1, l2) with
  | a :: l1', b :: l2' -> begin
      match compare a b with
      | -1 -> intersection l1' l2 (* a < b *)
      | 1 -> intersection l1 l2' (* a > b *)
      | _ -> 1 + intersection l1' l2
    end
  | _ -> 0
;;

(* Hum, ce n'est pas très propre. *)
let get_index arr e =
  let rec aux p = if arr.(p) = e then p else aux (p + 1) in
  aux 0
;;

let process_digits digits =
  (* On trie par longueur *)
  Array.sort
    (fun a b -> compare (String.length a) (String.length b))
    digits;
  let d = Array.make 10 [] in
  (* Les faciles *)
  d.(1) <- ordered_list_of_chars digits.(0);
  d.(7) <- ordered_list_of_chars digits.(1);
  d.(4) <- ordered_list_of_chars digits.(2);
  d.(8) <- ordered_list_of_chars digits.(9);
  (* Les 5 segments *)
  for i = 3 to 5 do
    let dd = ordered_list_of_chars digits.(i) in
    if intersection dd d.(1) = 2 then
      d.(3) <- dd
    else begin
      assert (intersection dd d.(1) = 1);
      if intersection dd d.(4) = 2 then
        d.(2) <- dd
      else begin
        assert (intersection dd d.(4) = 3);
        d.(5) <- dd
      end
    end
  done;
  (* Les 6 segments *)
  for i = 6 to 8 do
    let dd = ordered_list_of_chars digits.(i) in
    match
      (intersection dd d.(1), intersection dd d.(4))
    with
    | 2, 3 -> d.(0) <- dd
    | 1, 3 -> d.(6) <- dd
    | 2, 4 -> d.(9) <- dd
    | _ -> failwith "???"
  done;
  d
;;

let _ =
  let input = Scanf.Scanning.open_in "input_8.txt" in
  let s =
    stream_of_file input
      "%s %s %s %s %s %s %s %s %s %s | %s %s %s %s\n"
      (fun a b c d e f g h i j d1 d2 d3 d4 ->
        ( [| a; b; c; d; e; f; g; h; i; j |],
          [ d1; d2; d3; d4 ] ))
  in
  let somme = ref 0 in
  Stream.iter
    (fun (left, right) ->
      let digits = process_digits left in
      let number =
        List.fold_left
          (fun s d ->
            (10 * s)
            + get_index digits (ordered_list_of_chars d))
          0 right
      in
      somme := !somme + number)
    s;
  Scanf.Scanning.close_in input;
  Printf.printf "%d\n" !somme
;;
