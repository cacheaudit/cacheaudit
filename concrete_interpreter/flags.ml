(* Stores the values of the flags *)
open X86Types

type t = int array

let init n = Array.init n (fun _ -> 0)

let get_flag arr flg = Array.get arr (X86Util.flag_to_int flg)
let set_flag arr flg = Array.set arr (X86Util.flag_to_int flg)
let print_flags arr =
      let int_f_str x = X86Util.flag_to_string (X86Util.int_to_flag x) in
      Array.iteri (fun i x -> Format.printf "@[%s\t%x@]@." (int_f_str i) x) arr; print_newline ()
