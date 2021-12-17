(* Day 15 *)
(* On cherche un chemin. *)

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

type packet =
  | Litt of int * int
  | Oper of int * int * packet list

let int_of_hex c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | _ -> failwith "inf_of_hex"
;;

let read_file f =
  let input = Scanf.Scanning.open_in f in
  let hexa_stream = stream_of_file input "%c" int_of_hex in
  let bit_stream =
    let bit_count = ref 0
    and hexa_val = ref 0 in
    Stream.from (fun _ ->
        hexa_val := 2 * !hexa_val;
        if !bit_count = 0 then begin
          bit_count := 4;
          hexa_val := Stream.next hexa_stream
        end;
        decr bit_count;
        Some (!hexa_val land 8 = 8))
  in
  let bit_count = ref 0 in
  let read_bit () =
    incr bit_count;
    Stream.next bit_stream
  in
  let read_int n =
    let v = ref 0 in
    for i = 1 to n do
      v := 2 * !v;
      if read_bit () then incr v
    done;
    !v
  in
  let rec read_litt v =
    let continue = read_bit ()
    and value = (16 * v) + read_int 4 in
    if continue then read_litt value else value
  in
  let rec read_packet () =
    let versID = read_int 3 in
    let typeID = read_int 3 in
    if typeID = 4 then
      Litt (versID, read_litt 0)
    else
      Oper (versID, typeID, subpackages ())
  and subpackages () =
    let packet_list = ref [] in
    (if read_bit () then
       let packet_number = read_int 11 in
       for i = 1 to packet_number do
         packet_list := read_packet () :: !packet_list
       done
    else
      let bit_number = read_int 15 in
      let bit_stop = !bit_count + bit_number in
      while !bit_count < bit_stop do
        packet_list := read_packet () :: !packet_list
      done);
    List.rev !packet_list
  in
  read_packet ()
;;

let rec eval = function
  | Litt (_, v) -> v
  | Oper (_, o, l) -> begin
      match (o, l) with
      | 0, _ -> List.fold_left (fun a p -> a + eval p) 0 l
      | 1, _ -> List.fold_left (fun a p -> a * eval p) 1 l
      | 2, _ ->
          List.fold_left
            (fun a p -> min a (eval p))
            max_int l
      | 3, _ ->
          List.fold_left
            (fun a p -> max a (eval p))
            min_int l
      | 5, [ p1; p2 ] ->
          let e1 = eval p1
          and e2 = eval p2 in
          if e1 > e2 then 1 else 0
      | 6, [ p1; p2 ] ->
          let e1 = eval p1
          and e2 = eval p2 in
          if e1 < e2 then 1 else 0
      | 7, [ p1; p2 ] ->
          let e1 = eval p1
          and e2 = eval p2 in
          if e1 = e2 then 1 else 0
      | _ -> failwith "Unknown operator"
    end
;;

let _ =
  let p = read_file "input_16.txt" in
  Printf.printf "%d\n" (eval p)
;;
