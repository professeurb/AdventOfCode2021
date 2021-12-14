(* Day 14 *)
(* On déplie. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let rec update h = function
  | a :: b :: s -> (
      let s' = update h (b :: s) in
      match Hashtbl.find_opt h (a, b) with
      | None -> a :: s'
      | Some c -> a :: c :: s')
  | s -> s
;;

let _ =
  let input = Scanf.Scanning.open_in "input_14.txt" in
  (* Read starting template *)
  let template =
    Scanf.bscanf input "%s\n\n" (fun x -> x)
    |> String.to_seq |> List.of_seq
  in
  let rule_stream =
    (* @ is a symbol telling where to stop reading a string *)
    stream_of_file input "%c%c -> %c\n" (fun x y z ->
        (x, y, z))
  in
  let rules = Hashtbl.create 100 in
  Stream.iter
    (fun (x, y, z) -> Hashtbl.add rules (x, y) z)
    rule_stream;
  Scanf.Scanning.close_in input;
  (* Evolve polymer *)
  let polymer = ref template in
  for i = 1 to 10 do
    polymer := update rules !polymer
  done;
  (* Count letters *)
  let letter_count = Hashtbl.create 10 in
  List.iter
    (fun c ->
      Hashtbl.replace letter_count c
        (1
        +
        match Hashtbl.find_opt letter_count c with
        | None -> 0
        | Some cnt -> cnt))
    !polymer;
  let mini, maxi = (ref max_int, ref min_int) in
  Hashtbl.iter
    (fun _ cnt ->
      mini := min !mini cnt;
      maxi := max !maxi cnt)
    letter_count;
  Printf.printf "%d\n" (!maxi - !mini)
;;
