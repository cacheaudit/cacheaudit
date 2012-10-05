(** Simulate a cache **) 
(* stores addresses and not real content *)

type hm_t =
    Hit of int64
  | Miss of int64

type cache_t = 
{
  cache_size : int; (* in bytes *)
  line_size : int; (* in bytes *)
  associativity : int;
  num_sets : int; (* this field is redundant, can be computed from previous three *)
  content : int64 option list array;
  marked : bool array;
  mutable verbose : bool;
  trace : hm_t Queue.t
}

let hm_to_string = function
    Hit x -> Printf.sprintf "Hit 0x%08Lx" x
  | Miss x -> Printf.sprintf "Miss 0x%08Lx" x

let gen_empty_aset n = 
  let rec gen_list n elt = match n with
  | 0 -> []
  | n -> elt :: (gen_list (n-1) elt)
  in gen_list n None

let init cs ls ass =
  let ns = cs / ls / ass in
  {
    cache_size = cs;
    line_size = ls;
    associativity = ass;
    num_sets = ns;
    content = Array.init ns (fun _ -> gen_empty_aset ass);
    marked = Array.init ns (fun _ -> false);
    verbose = false;
    trace = Queue.create ()
  }

(* Determine the set in which an address is cached *)
(* calculated addr mod num_sets *)
(* addr shouldn't be negative as no real modulo is used *)
let get_set_addr cache addr =  Int64.to_int (Int64.rem addr (Int64.of_int cache.num_sets))

let get_block_addr cache addr = Int64.div addr (Int64.of_int cache.line_size)



exception Empty_list
let update_aset aset v =
  let rec update sofar rest v = match rest with
  | None :: xs -> v :: (List.append sofar xs) (* xs should contain only None's *)
  | x :: [] -> v :: sofar
  | x :: xs -> 
      if x = v then v :: List.append sofar xs
      else update (List.append sofar [x]) xs v
  | [] -> raise Empty_list
  in update [] aset v

let print_cache cache mark hm =
  let print_elem e = print_char ' '; 
  (match e with
    | None -> Printf.printf "%12s" "⊥"
    | Some x ->  Printf.printf "0x%08Lx" x);
  print_string " |"
  in
  let print_list xs = Printf.printf "|"; List.iter print_elem xs in
  let hm_str = hm_to_string hm in
  Printf.printf "Cache %s " hm_str;
  for i = 0 to (13 * cache.associativity - String.length hm_str) do
          print_char '-'
  done;
  print_newline ();
  Array.iteri (fun i l -> 
          if cache.marked.(i) then
                  (if i = mark then print_char '*' else print_char ' ';
                  Printf.printf " 0x%02X " (i + 1);
                  print_list l; print_newline ())
          else ())
          cache.content;
          print_newline ()

let write cache addr = 
	let addr = get_block_addr cache addr in
  let i = get_set_addr cache addr in
  let hitmiss = if List.exists (fun x -> Some addr = x) cache.content.(i) then Hit addr else Miss addr in
  Array.set cache.content i (update_aset cache.content.(i) (Some addr));
  Array.set cache.marked i true;
  Queue.add hitmiss cache.trace;
  if cache.verbose then print_cache cache i hitmiss

let set_verbose cache v = cache.verbose <- v

(*(* example usage *)
let cache_size = 16384
let line_size = 32
let associativity = 4
let cache = init cache_size line_size associativity*)
