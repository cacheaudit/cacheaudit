(* Imperative style stack, with in place modifications *)

exception Stack_empty

type t = 
{ 
  mutable content: Int64.t array;
  mutable size   : int;
  mutable ebp : int64;
  mutable esp : int64;
  mutable verbose : bool
}

let init addr = {content = [||]; size = 0; ebp = -1L; esp = -1L; verbose = false}

let set_ebp st addr = st.ebp <- addr; st.esp <- addr

let new_size s = if s < 1000 then s*2+1 else s+1000

let grow st = 
  let new_content = Array.make (new_size (Array.length st.content)) Int64.zero
  in Array.blit st.content 0 new_content 0 st.size;
  st.content <- new_content

(* Translate real address to address in stack *)
let to_saddr st raddr =  
  let saddr = Int64.to_int (Int64.div (Int64.sub st.ebp raddr) 4L) in
  if Int64.rem (Int64.sub st.ebp raddr) 4L <> 0L && st.verbose then  
    Printf.printf "Unaligned address 0x%Lx -> %d - Rem %Ld\n" raddr saddr (Int64.rem (Int64.sub st.ebp raddr) 4L);
  saddr
  
(* Translate stack address to real address *)
let to_raddr st saddr = Int64.sub st.ebp (Int64.of_int (saddr * 4))

let get st addr = let saddr = to_saddr st addr in
  if st.verbose then Printf.printf "Get word in stack address 0x%Lx (%d): 0x%Lx\n" addr saddr (Array.get st.content saddr);
  Array.get st.content saddr
let set st addr value =  let saddr = to_saddr st addr in
  if st.verbose then Printf.printf "Set word in stack address 0x%Lx (%d): 0x%Lx\n" addr saddr value;
  if addr = st.esp && st.size >= Array.length st.content then grow st;
  Array.set st.content saddr value

let push st x = if st.size >= Array.length st.content then grow st;
      set st (to_raddr st st.size) x;
      st.size <- st.size + 1;
      st.esp <- Int64.sub st.esp 4L

let pop st = if st.size = 0 then raise Stack_empty else 
      st.size <- st.size -1;
      st.esp <- Int64.add st.esp 4L;
      get st (to_raddr st st.size)

let update_esp st new_esp =
  if new_esp < st.esp then
    for i = 1 to  Int64.to_int(Int64.div (Int64.sub st.esp new_esp) 4L) do 
      push st 0L
    done
  else
    for i = 1 to  Int64.to_int(Int64.div (Int64.sub new_esp st.esp) 4L) do 
      ignore (pop st)
    done;
  assert (st.esp = new_esp);
  assert (st.size = Int64.to_int(Int64.div (Int64.sub st.ebp st.esp) 4L))

let get_byte st addr =
  let off = Int64.to_int (Int64.rem (Int64.sub st.ebp addr) 4L) in
  let saddr = (if off <> 0 then 1 else 0) + to_saddr st addr in
  let res = Int64.logand (0xFFL) (Int64.shift_right (Array.get st.content saddr)  (32 - (off + 1) * 8)) in
  if st.verbose then Printf.printf "Get word in stack address 0x%Lx (%d): 0x%Lx - Offset: %d - Byte returned: 0x%Lx\n" 
  (to_raddr st saddr) saddr (Array.get st.content saddr) off res;
  res

let set_byte st addr value =
  let off = Int64.to_int (Int64.rem (Int64.sub st.ebp addr) 4L) in
  let saddr = (if off <> 0 then 1 else 0) +  to_saddr st addr in
  let mask = Int64.lognot (Int64.shift_left 0xFFL (32 - (off + 1) * 8)) in
  let sval = Int64.shift_left (Int64.logand value 0xFFL) (32 - (off + 1) * 8) in
  let res = Int64.logor (Int64.logand mask (Array.get st.content saddr)) sval in
  if st.verbose then Printf.printf "Set word in stack address 0x%Lx (%d): 0x%Lx - Offset: %d - Byte written: 0x%Lx - New word: 0x%Lx\n"
  (to_raddr st saddr) saddr (Array.get st.content saddr) off (Int64.logand value 0xFFL) res;
  Array.set st.content saddr res
  

let is_empty st = st.size = 0

let empty_stack st = st.size <- 0

let in_stack st addr = 
  addr <= st.ebp && addr >= Int64.sub st.ebp (Int64.of_int (st.size * 4))

let set_verbose st verb = st.verbose <- verb
