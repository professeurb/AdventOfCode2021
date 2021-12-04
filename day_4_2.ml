(* Day 4 *)
(* On joue au Bingo. *)

let id x = x

exception Winning_board of int

(* On ne programme vraiment pas de façon
   élégante ou efficace. *)

let play_board board num =
  (* On recherche si num est présent dans board,
     et si c'est le cas, on note sa position. *)
  let pos = ref None in
  for i = 0 to 4 do
    for j = 0 to 4 do
      if board.(i).(j) = num then pos := Some (i, j)
    done
  done;
  match !pos with
  | None -> ()
  | Some (l, c) ->
      (* Si on l'a trouvé, on le marque
         puis on regarde on a complété
         une ligne ou une colonne... *)
      board.(l).(c) <- -1;
      let ligne = ref true
      and colonne = ref true in
      for i = 0 to 4 do
        if board.(i).(c) >= 0 then ligne := false;
        if board.(l).(i) >= 0 then colonne := false
      done;
      (* Si c'est le cas, on calcule le score
         et on lève une exception pour le transmettre. *)
      if !ligne || !colonne then begin
        let cnt = ref 0 in
        for i = 0 to 4 do
          for j = 0 to 4 do
            if board.(i).(j) >= 0 then
              cnt := !cnt + board.(i).(j)
          done
        done;
        raise (Winning_board (num * !cnt))
      end
;;

let _ =
  let input = Scanf.Scanning.open_in "input_4.txt" in
  try
    (* Lecture des tirages *)
    let draw = ref [] in
    draw := Scanf.bscanf input "%d" id :: !draw;
    (try
       while true do
         draw := Scanf.bscanf input ",%d" id :: !draw
       done
     with Stdlib.Scanf.Scan_failure _ -> ());
    draw := List.rev !draw;
    (* Il reste un passage à la ligne *)
    Scanf.bscanf input "\n" ();
    (* On lit les plateaux *)
    let boards = ref [] in
    (try
       while true do
         Scanf.bscanf input "\n" ();
         let board = Array.make 5 [||] in
         for i = 0 to 4 do
           board.(i) <-
             Scanf.bscanf input " %d %d %d %d %d\n"
               (fun a b c d e -> [| a; b; c; d; e |])
         done;
         boards := board :: !boards
       done
     with End_of_file -> ());
    Scanf.Scanning.close_in input;
    (* Maintenant, on simule les tirages.
       Dès qu'un plateau est gagnant, on note
       son score et on le retire de la liste
       des plateaux. *)
    let final_score = ref 0
    and next_boards = ref [] in
    List.iter
      (fun num ->
        next_boards := [];
        List.iter
          (fun board ->
            try
              play_board board num;
              next_boards := board :: !next_boards
            with Winning_board score ->
              final_score := score)
          !boards;
        boards := !next_boards)
      !draw;
    (* C'est le score du dernier plateau gagnant. *)
    Printf.printf "%d\n" !final_score
  with e ->
    Scanf.Scanning.close_in input;
    raise e
;;
