(* Day 14 *)
(* On déplie. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let pos l e =
  let rec aux l p =
    match l with
    | [] -> failwith "pos"
    | a :: l' -> if a = e then p else aux l' (p + 1)
  in
  aux l 0
;;

(* Matrix product *)
let prod m1 m2 =
  let n = Array.length m1 in
  let mr = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      for k = 0 to n - 1 do
        mr.(i).(j) <- mr.(i).(j) + (m1.(i).(k) * m2.(k).(j))
      done
    done
  done;
  mr
;;

(* Fast matrix exponentiation *)
let rec pow m n =
  if n = 1 then
    m
  else
    let m2 = pow m (n / 2) in
    if n mod 2 = 0 then
      prod m2 m2
    else
      prod m (prod m2 m2)
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
  let rules = ref [] in
  Stream.iter (fun r -> rules := r :: !rules) rule_stream;
  Scanf.Scanning.close_in input;
  (* Get list of letters *)
  let letters =
    List.sort_uniq compare
      (List.fold_left
         (fun l (a, b, c) -> a :: b :: c :: l)
         [] !rules)
  in
  let n = List.length letters in
  (* Make matrix representing rules *)
  let n2 = n * n in
  let mat = Array.make_matrix n2 n2 0 in
  for i = 0 to n2 - 1 do
    mat.(i).(i) <- 1
  done;
  List.iter
    (fun (a, b, c) ->
      let na = pos letters a
      and nb = pos letters b
      and nc = pos letters c in
      let nab = (10 * na) + nb
      and nac = (10 * na) + nc
      and ncb = (10 * nc) + nb in
      mat.(nab).(nab) <- 0;
      mat.(nac).(nab) <- 1;
      mat.(ncb).(nab) <- 1)
    !rules;
  (* Evolve polymer *)
  let m40 = pow mat 40 in
  (* Count letters *)
  let letter_count = Array.make n 0 in
  let first_letter = pos letters (List.hd template) in
  letter_count.(first_letter) <- 1;
  let rec scan_template = function
    | a :: b :: s ->
        let na = pos letters a
        and nb = pos letters b in
        let nab = (10 * na) + nb in
        for i = 0 to n2 - 1 do
          let last_letter = i mod n in
          letter_count.(last_letter) <-
            letter_count.(last_letter) + m40.(i).(nab)
        done;
        scan_template (b :: s)
    | _ -> ()
  in
  scan_template template;
  let maxi = ref letter_count.(0)
  and mini = ref letter_count.(0) in
  for i = 1 to n - 1 do
    maxi := max !maxi letter_count.(i);
    mini := min !mini letter_count.(i)
  done;
  Printf.printf "%d\n" (!maxi - !mini)
;;
