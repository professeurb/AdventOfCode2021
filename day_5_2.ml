(* Day 5 *)
(* On évite les sources hydrothermales. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

(* On utilise un type somme avec enregistrement,
   c'est plus clair. *)

type ligne =
  | H of { x1 : int; x2 : int; y : int }
  | V of { x : int; y1 : int; y2 : int }
  | D of { x : int; y : int; l : int; d : int }

(* Plutôt que d'utiliser une matrice de points,
   nous allons utiliser un dictionnaire pour enregistrer
   les points couverts et s'ils sont couverts plusieurs fois
   à l'aide d'un booléen. *)

(* Dans une première version, on représente les dictionnaires
   à l'aide d'arbres binaires de recherche. C'est une version
   persistente, et les interactions avec le dictionnaire se font
   en temps logarithmique.
*)
module VersionFonctionnelle = struct
  module Coords = struct
    type t = int * int

    let compare ((x1, y1) : t) ((x2, y2) : t) =
      match compare x1 x2 with
      | 0 -> compare y1 y2
      | v -> v
    ;;
  end

  module Carte = Map.Make (Coords)

  let traite_point x y carte cnt =
    match Carte.find_opt (x, y) !carte with
    | None -> carte := Carte.add (x, y) false !carte
    | Some false ->
        incr cnt;
        carte := Carte.add (x, y) true !carte
    | _ -> ()
  ;;

  let compte_source () =
    let input = Scanf.Scanning.open_in "input_5.txt" in
    try
      let s =
        stream_of_file input "%d,%d -> %d,%d\n"
          (fun x1 y1 x2 y2 ->
            if x1 = x2 then
              V { x = x1; y1 = min y1 y2; y2 = max y1 y2 }
            else if y1 = y2 then
              H { x1 = min x1 x2; x2 = max x1 x2; y = y1 }
            else if x1 < x2 then
              D
                {
                  x = x1;
                  y = y1;
                  l = x2 - x1;
                  d = (if y1 < y2 then 1 else -1);
                }
            else
              D
                {
                  x = x2;
                  y = y2;
                  l = x1 - x2;
                  d = (if y2 < y1 then 1 else -1);
                })
      in
      let carte = ref Carte.empty
      and cnt = ref 0 in
      Stream.iter
        (fun l ->
          match l with
          | H { x1; x2; y } ->
              for x = x1 to x2 do
                traite_point x y carte cnt
              done
          | V { x; y1; y2 } ->
              for y = y1 to y2 do
                traite_point x y carte cnt
              done
          | D { x; y; l; d } ->
              for i = 0 to l do
                traite_point (x + i) (y + (i * d)) carte cnt
              done)
        s;
      !cnt
    with e ->
      Scanf.Scanning.close_in input;
      raise e
  ;;
end

(* Deuxième version avec une table de hachage. On perd le
   côté persistent (qui ne nous sert pas ici) et on gagne en
   vitesse, puisque les interactions se font maintenant
   en temps constant en moyenne. *)
module Version_Imperative = struct
  let traite_point x y carte cnt =
    match Hashtbl.find_opt carte (x, y) with
    | None -> Hashtbl.add carte (x, y) false
    | Some false ->
        incr cnt;
        Hashtbl.add carte (x, y) true
    | _ -> ()
  ;;

  let compte_source () =
    let input = Scanf.Scanning.open_in "input_5.txt" in
    try
      let s =
        stream_of_file input "%d,%d -> %d,%d\n"
          (fun x1 y1 x2 y2 ->
            if x1 = x2 then
              V { x = x1; y1 = min y1 y2; y2 = max y1 y2 }
            else if y1 = y2 then
              H { x1 = min x1 x2; x2 = max x1 x2; y = y1 }
            else if x1 < x2 then
              D
                {
                  x = x1;
                  y = y1;
                  l = x2 - x1;
                  d = (if y1 < y2 then 1 else -1);
                }
            else
              D
                {
                  x = x2;
                  y = y2;
                  l = x1 - x2;
                  d = (if y2 < y1 then 1 else -1);
                })
      in
      let carte = Hashtbl.create 200
      and cnt = ref 0 in
      Stream.iter
        (fun l ->
          match l with
          | H { x1; x2; y } ->
              for x = x1 to x2 do
                traite_point x y carte cnt
              done
          | V { x; y1; y2 } ->
              for y = y1 to y2 do
                traite_point x y carte cnt
              done
          | D { x; y; l; d } ->
              for i = 0 to l do
                traite_point (x + i) (y + (i * d)) carte cnt
              done)
        s;
      !cnt
    with e ->
      Scanf.Scanning.close_in input;
      raise e
  ;;
end

let _ =
  Printf.printf "%d\n" (Version_Imperative.compte_source ())
;;
