(* Imperative style stack, with in place modifications *)

exception Stack_empty

type t = { mutable content: Int64.t array;
           mutable size   : int }

let empty = {content = [||]; size = 0}

let new_size s = if s < 1000 then s*2+1 else s+1000

let grow st = 
  let new_content = Array.make (new_size (Array.length st.content)) Int64.zero
  in Array.blit st.content 0 new_content 0 st.size;
  st.content <- new_content

let get st = Array.get st.content
let set st = Array.set st.content

let push st x = if st.size >= Array.length st.content then grow st;
      set st st.size x;
      st.size <- st.size + 1

let pop st = if st.size = 0 then raise Stack_empty else st.size <- st.size -1;
      get st st.size

let is_empty st = st.size = 0

let empty_stack st = st.size <- 0
