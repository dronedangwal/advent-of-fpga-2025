open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Day_01

let ( <--. ) = Bits.( <--. )

let read_input filename =
  let ic = In_channel.create filename in
  let rec loop acc =
    match In_channel.input_line ic with
    | Some line ->
        if String.length line < 2 then
          failwith ("Invalid line: " ^ line);
        let c = line.[0] in
        let num =
          int_of_string (String.sub line ~pos:1 ~len:(String.length line - 1))
        in
        let x, y = Char.to_int c, num in
          loop ((x, y) :: acc)
    | None ->
        In_channel.close ic;
        (Char.to_int 'R', 50) :: (List.rev ((Char.to_int 'R', 0) :: ((Char.to_int 'R', 0) :: acc)))
  in
  loop [];;

let sample_input_values = 
  Stdio.printf("Reading input file...\n");
  read_input "../input.txt";;

let%expect_test "Sample test" =
  (* 1. Setup Simulation *)
  let scope = Scope.create () in

  (* A. Create the Circuit *)
  let module C = Circuit.With_interface (Day_01.I) (Day_01.O) in
  let circuit = C.create_exn ~name:"day_01" (Day_01.create scope) in

  (* B. Create a GENERIC simulator *)
  let sim = Cyclesim.create circuit in

  (* C. Look up ports by name *)
  let in_a           = Cyclesim.in_port sim "direction" in
  let in_b           = Cyclesim.in_port sim "clicks" in
  let in_data_valid  = Cyclesim.in_port sim "data_valid" in
  let in_rst         = Cyclesim.in_port sim "rst" in

  let out_dial = Cyclesim.out_port sim "dial" in
  let out_part1 = Cyclesim.out_port sim "part1" in
  let out_at_zero = Cyclesim.out_port sim "at_zero" in
  let out_excess_zeros = Cyclesim.out_port sim "excess_zeros" in

  (* 2. Reset *)
  Cyclesim.reset sim;
  in_rst := Bits.vdd;
  Cyclesim.cycle sim;
  in_rst := Bits.gnd;

  (* 3. Drive Simulation *)
  List.iter sample_input_values ~f:(fun (x, y) ->
      in_data_valid := Bits.vdd;
      in_a := Bits.of_int_trunc ~width:Day_01.num_bits x;
      in_b := Bits.of_int_trunc ~width:Day_01.num_bits y;
      Cyclesim.cycle sim
  );

  (* 4. Settle *)
  in_data_valid := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;

  (* 5. Print Result *)
  let position = Bits.to_signed_int !out_dial in
  let part1 = Bits.to_signed_int !out_part1 in
  let at_zero_count = Bits.to_unsigned_int !out_at_zero in
  let excess_zero_count = Bits.to_unsigned_int !out_excess_zeros in
  Stdio.printf "Final position of dial: %d\n" position;
  Stdio.printf "Part 1 Password: %d\n" part1;
  Stdio.printf "Part 2 Password: %d\n" (at_zero_count + excess_zero_count);
  
  (* 6. Expectation *)
  [%expect {||}]
;;
