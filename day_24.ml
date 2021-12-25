(* Day 24 *)
(* On fait du reverse engineering. *)

(* On a à chaque fois un registre plus une constante. *)

type arg = Reg of int | Cst of int

type instr =
  | Inp of int
  | Add of int * arg
  | Mul of int * arg
  | Div of int * arg
  | Mod of int * arg
  | Eql of int * arg

let get_reg c =
  assert (String.length c = 1);
  Char.code c.[0] - Char.code 'w'
;;

let get_arg c =
  match c.[0] with
  | 'w' .. 'z' -> Reg (get_reg c)
  | _ -> Cst (int_of_string c)
;;

let read_instruction s =
  let l = String.split_on_char ' ' s in
  match l with
  | [ "inp"; a ] -> Inp (get_reg a)
  | [ "add"; a; b ] -> Add (get_reg a, get_arg b)
  | [ "mul"; a; b ] -> Mul (get_reg a, get_arg b)
  | [ "div"; a; b ] -> Div (get_reg a, get_arg b)
  | [ "mod"; a; b ] -> Mod (get_reg a, get_arg b)
  | [ "eql"; a; b ] -> Eql (get_reg a, get_arg b)
  | _ -> failwith ("read_instructions :" ^ s)
;;

exception BadInstr of instr

let simul instructions =
  let eqs = ref [] in
  let z = ref [ (-1, 0) ]
  and x = ref (0, 0)
  and y = ref (0, 0)
  and w = ref 0
  and pos = ref (-1)
  and red = ref false in
  List.iter
    (function
      | Inp 0 ->
          incr pos;
          w := !pos
      | Mul (1, Cst 0) -> ()
      | Add (1, Reg 3) -> x := List.hd !z
      | Mod (1, Cst 26) -> ()
      | Div (3, Cst 1) -> red := false
      | Div (3, Cst 26) ->
          red := true;
          z := List.tl !z
      | Add (1, Cst c) ->
          let a, b = !x in
          x := (a, b + c)
      | Eql (1, Reg 0) ->
          if !red then
            let a, b = !x in
            eqs :=
              (if b >= 0 then (a, b, !w) else (!w, -b, a))
              :: !eqs
      | Eql (1, Cst 0) -> ()
      | Mul (2, Cst 0) -> ()
      | Add (2, Cst c) -> y := (!w, c)
      | Mul (2, Reg 1) -> ()
      | Mul (3, Reg 2) -> ()
      | Add (2, Reg 0) -> ()
      | Add (3, Reg 2) -> if not !red then z := !y :: !z
      | i -> raise (BadInstr i))
    instructions;
  !eqs
;;

let mini eqs =
  let res = Array.make 14 0 in
  List.iter
    (fun (a, b, c) ->
      res.(a) <- 1;
      res.(c) <- 1 + b)
    eqs;
  res
;;

let maxi eqs =
  let res = Array.make 14 0 in
  List.iter
    (fun (a, b, c) ->
      res.(a) <- 9 - b;
      res.(c) <- 9)
    eqs;
  res
;;

let stream_of_file input patt action =
  let streamer _ =
    try Some (Scanf.bscanf input patt action)
    with End_of_file -> None
  in
  Stream.from streamer
;;

let _ =
  let input = Scanf.Scanning.open_in "input_24.txt" in
  let inst_stream =
    stream_of_file input "%s@\n" read_instruction
  in
  let instructions = ref [] in
  (try
     Stream.iter
       (fun i -> instructions := i :: !instructions)
       inst_stream
   with Failure _ -> ());
  Scanf.Scanning.close_in input;
  let instructions = List.rev !instructions in
  let eqs = simul instructions in
  print_string "Max : ";
  Array.iter print_int (maxi eqs);
  print_string "\n";
  print_string "Min : ";
  Array.iter print_int (mini eqs);
  print_string "\n"s
;;
