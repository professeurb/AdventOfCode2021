(* Day 3 *)
(* On travaille la manipulation de représentations binaires. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

(* Comptage du nombre d'éléments
   et du nombre de 1 en position pos. *)
let count list pos =
  let rec aux (ones, total) l =
    match l with
    | [] -> (ones, total)
    | number :: l' ->
        if number.[pos] = '1' then
          aux (ones + 1, total + 1) l'
        else
          aux (ones, total + 1) l'
  in
  aux (0, 0) list
;;

(* Conversion de la représentation en binaire
   d'un entier en l'entier lui-même, à la Horner. *)
let int_of_binstring s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do
    n := 2 * !n;
    if s.[i] = '1' then incr n
  done;
  !n
;;

(* Un mot sur le test dans la fonction suivante.
     Si on calcule l'ogr, on sélectionne les '1' quand
     on a ones >= zeros, soit 2 * ones >= tot.
     Si on calcule le csr, on sélectionne les '1' quand
     on a ones < zeros, soit 2 * ones < tot.
     Pour savoir quand on sélectionne en gardant les '1', on fait
     donc un "ou exclusif" entre les booléens 2 * ones >= tot
     et is_csr, lequel "ou exclusif" est traduit par l'inégalité
   entre deux booléens.
*)
let rec filter_list rates pos is_csr =
  match rates with
  | [ rate ] -> int_of_binstring rate
  | [] -> failwith "No more rates"
  | _ ->
      let ones, tot = count rates pos in
      let new_rates =
        List.filter
          (* On peut mettre un if n'importe où
             en programmation fonctionnelle. *)
          (if 2 * ones >= tot <> is_csr then
             fun s -> s.[pos] = '1'
          else
            fun s -> s.[pos] = '0')
          rates
      in
      filter_list new_rates (pos + 1) is_csr
;;

let _ =
  let input = Scanf.Scanning.open_in "input_3.txt" in
  try
    let s = stream_of_file input "%s\n" (fun a -> a) in
    (* Make list of digits *)
    let report = ref [] in
    Stream.iter (fun s -> report := s :: !report) s;
    (* Filter lists and get rates *)
    let ogr = filter_list !report 0 false
    and csr = filter_list !report 0 true in
    Printf.printf "%d\n" (ogr * csr)
  with e ->
    Scanf.Scanning.close_in input;
    raise e
;;
