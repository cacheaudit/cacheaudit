(* Stores the values of the registers *)
open X86Util
open Printf

type t = Int64.t array

let empty n = Array.init n (fun _ -> Int64.zero)

let get_value_32 arr reg = Array.get arr (reg32_to_int reg)
let set_value_32 arr reg value =
      let mask = Int64.of_string "0xFFFFFFFF" in
      Array.set arr (reg32_to_int reg) (Int64.logand value mask)

let get_value_16 arr reg =
      let x_reg = Array.get arr (reg16_to_int reg) in
      Int64.shift_right_logical x_reg 16
let set_value_16 arr reg value =
      let reg_num = reg16_to_int reg in
      let value_shift = Int64.shift_left value 16 in
      let reg_masked = Int64.logand (Array.get arr reg_num) (Int64.of_int 0x0000FFFF) in
      Array.set arr reg_num (Int64.logor reg_masked value_shift)

let get_value_8 arr reg =
      let reg_num = reg8_to_int reg in
      let hilo = if reg_num < 4 then (0, 0xFF) (* Low *) else (8, 0xFF00) (* High *) in
      let hl_reg = Array.get arr (reg_num mod 4) in
      let reg_masked = Int64.logand hl_reg (Int64.of_int (snd hilo)) in
      Int64.shift_right_logical reg_masked (fst hilo)
let set_value_8 arr reg value =
      let reg_num = reg8_to_int reg in
      let hilo = if reg_num < 4 then (0, "0xFFFFFF00") else (8, "0xFFFF00FF") in
      let hl_reg = Array.get arr (reg_num mod 4) in
      let value_shift = Int64.shift_left value (fst hilo) in
      let reg_masked = Int64.logand hl_reg (Int64.of_string (snd hilo)) in
      Array.set arr reg_num (Int64.logor reg_masked value_shift)

let print_regs arr =
      let int_r32_str x = reg32_to_string (int_to_reg32 x) in
      Array.iteri (fun i x -> printf "\t%s %Ld" (int_r32_str i) x) arr; print_newline ()

let get_segment arr reg = Array.get arr (segment_reg_to_int reg)
let set_segment arr reg value =
      let mask = Int64.of_int 0xFFFF in
      Array.set arr (segment_reg_to_int reg) (Int64.logand value mask)
