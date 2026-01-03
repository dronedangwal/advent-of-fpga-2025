open Core
open Hardcaml
open Signal

let num_bits = 16;;

module I = struct
  type 'a t =
    { clk   : 'a
    ; rst   : 'a
    ; direction     : 'a [@bits num_bits]
    ; clicks        : 'a [@bits num_bits]
    ; data_valid: 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { dir : 'a [@bits 1]
    ; quotient : 'a [@bits num_bits]
    ; remainder : 'a [@bits num_bits]
    ; dial : 'a [@bits num_bits]
    ; part1 : 'a [@bits num_bits]
    ; at_zero: 'a [@bits num_bits]
    ; excess_zeros: 'a [@bits num_bits]
    }
  [@@deriving hardcaml]
end

let create scope (i : _ I.t): _ O.t =

  let spec = Reg_spec.create ~clock:i.clk ~clear:i.rst () in
  let open Always in

  (* registers *)

  let%hw_var dir = Variable.reg spec ~width:1 in
  let%hw_var quotient = Variable.reg spec ~width:num_bits in
  let%hw_var remainder = Variable.reg spec ~width:num_bits in
  let%hw_var dial = Variable.reg spec ~width:num_bits in
  let%hw_var part1 = Variable.reg spec ~width:num_bits in
  let%hw_var at_zero = Variable.reg spec ~width:num_bits in
  let%hw_var excess_zeros = Variable.reg spec ~width:num_bits in

  (* helper functions *)

  let divby100_pos scope (x : Signal.t) : Signal.t =
    (* 
      Return the quotient when x (an unsigned int) is divided by 100
      Input: 
        scope: Scope.t
        x: Signal.t interpreted as the unsigned integer to be divided
      Output:
        The unsigned integer quotient [x/100] as a signal of same width as x 
    *)
    let w = width x in
    let%hw x in
    let rec divide_by_100 (v: Signal.t) (pow: int) (q: int) =
      if pow = -1 then
        of_unsigned_int ~width:w q
      else mux (v >=:. 100 * (Int.pow 2 pow)) [
        divide_by_100 v (pow - 1) q ;
        divide_by_100 (v -:. 100 * (Int.pow 2 pow)) (pow - 1) (q + Int.pow 2 pow)
      ]
    in
    divide_by_100 (uresize ~width:(w + 8) x) (w - 1) 0
  in

  let divby100_neg scope (x : Signal.t) : Signal.t =
    let w = width x in
    let%hw x in
    let%hw y = ~:(x -:. 1) in (* |y| = abs |x| *)
    let%hw z = of_signed_int ~width:w (-1) -: divby100_pos scope y in
    mux (x -: uresize ~width:w (of_signed_int ~width:w 100 *+ z) ==:. 100) [
      z ;
      z +:. 1
    ]
  in

  let quotient_100 scope (x : Signal.t) : Signal.t =
    let%hw x in
    mux (x >=+. 0) [
      divby100_neg scope x ;
      divby100_pos scope x
    ]
  in

  let remainder_100 scope (x : Signal.t) : Signal.t =
    let w = width x in
    let%hw x in
    x -: uresize ~width:w (of_signed_int ~width:w 100 *+ quotient_100 scope x)
  in

  compile
    [
      when_ i.rst [
        dir <-- zero 1
        ; quotient <-- zero num_bits
        ; remainder <-- zero num_bits
        ; dial <-- zero num_bits
        ; at_zero <-- zero num_bits
        ; excess_zeros <-- zero num_bits
      ] ;
      when_ i.data_valid [

        excess_zeros <-- excess_zeros.value +: quotient.value ;
        when_ (dir.value ==:. 0) [
          when_ ((dial.value >+. 0) &: (remainder.value >=+ dial.value)) [
            at_zero <-- at_zero.value +:. 1
            ] ;
            dial <-- remainder_100 scope (dial.value -: remainder.value)
        ] ;
        when_ (dir.value ==:. 1) [
          when_ ((dial.value >+. 0) &: (remainder.value +: dial.value >=+. 100)) [
            at_zero <-- at_zero.value +:. 1
          ] ;
          dial <-- remainder_100 scope (dial.value +: remainder.value)
        ] ;
          
        dir <-- mux (i.direction ==: of_string "0000000001001100") [
          of_string "1" ;
          of_string "0"
        ] ;
        quotient <-- quotient_100 scope i.clicks ;
        remainder <-- remainder_100 scope i.clicks ;
        part1 <-- part1.value +: mux (dial.value ==:. 0) [of_unsigned_int ~width:num_bits 0 ; of_unsigned_int ~width:num_bits 1]
      ] ;
    ];
    { O.dir = dir.value; O.quotient = quotient.value; O.remainder = remainder.value; O.dial = dial.value; 
    O.part1 = part1.value -:. 2; O.excess_zeros = excess_zeros.value; O.at_zero = at_zero.value}
          
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day-01" create
;;
