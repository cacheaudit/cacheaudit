(* Stores the values of the flags *)
open X86Types

type t = int array

let empty n = Array.init n (fun _ -> 0)

let get_flag arr flg = Array.get arr (X86Util.flag_to_int flg)
let set_flag arr flg = Array.set arr (X86Util.flag_to_int flg)
