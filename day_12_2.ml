(* Day 12 *)
(* On parcourt un graphe... euh non, une caverne. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let is_upper s = Char.code s.[0] <= 90

let rec parcours hash u path cnt =
  if u = "end" then
    incr cnt
  else
    List.iter
      (fun v ->
        if is_upper v || not (List.mem v path) then
          parcours hash v (u :: path) cnt)
      (Hashtbl.find hash u)
;;

let rec parcours2 hash u path cnt =
  if u = "end" then
    incr cnt
  else
    List.iter
      (fun v ->
        if is_upper v || not (List.mem v path) then
          parcours2 hash v (u :: path) cnt
        else if v <> "start" then
          parcours hash v (u :: path) cnt)
      (Hashtbl.find hash u)
;;

(* On a une liste vide par défaut. *)
let get_list h k =
  match Hashtbl.find_opt h k with
  | None -> []
  | Some l -> l
;;

let _ =
  let input = Scanf.Scanning.open_in "input_12.txt" in
  let s =
    (* @ is a symbol telling where to stop reading a string *)
    stream_of_file input "%s@-%s\n" (fun s t -> (s, t))
  in
  let h = Hashtbl.create 10 in
  Stream.iter
    (fun (s, t) ->
      Hashtbl.add h s (t :: get_list h s);
      Hashtbl.add h t (s :: get_list h t))
    s;
  let cnt = ref 0 in
  parcours2 h "start" [] cnt;
  Scanf.Scanning.close_in input;
  Printf.printf "%d\n" !cnt
;;
