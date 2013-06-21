type log_level = Quiet | Normal | Debug

type ad = AgeLL | ArchitectureLL | CacheLL | FlagLL | MemLL | OctLL | RelCacheLL | SimpleOctLL | SimpleRelSetLL | StackLL | TraceLL | ValLL | IteratorLL

module ADMap = Map.Make(struct type t = ad let compare = Pervasives.compare end )

let global_log_level = ref Quiet

let ad_log_level = ref ADMap.empty

let string_to_ad = function
	| "ageAD" -> AgeLL
	| "architectureAD" -> ArchitectureLL
	| "cacheAD" -> CacheLL
	| "flagAD" -> FlagLL
	| "memAD" -> MemLL
	| "octAD" -> OctLL
	| "relCacheAD" -> RelCacheLL
	| "simpleOctAD" -> SimpleOctLL
	| "simpleRelSetAD" -> SimpleRelSetLL
	| "stackAD" -> StackLL
	| "traceAD" -> TraceLL
	| "valAD" -> ValLL
	| "iterator" -> IteratorLL
	| _ -> failwith "AD not recognized"

let string_to_log_level = function
	| "normal" -> Normal
	| "quiet" -> Quiet
	| "debug" -> Debug
	| _ -> failwith "log level not recognized. Options: quiet, normal or debug"

let ad_dependencies = function
	| ValLL -> [FlagLL]
	| _ -> []

let set_log_level ad level = 
	let rec update_dep ad =
		List.iter (fun x ->
			if ADMap.mem x !ad_log_level then
				()
			else begin
				ad_log_level := ADMap.add x Normal !ad_log_level;
				update_dep x
			end
			) (ad_dependencies ad); () in
	 ad_log_level := ADMap.add ad level !ad_log_level;
	 if level <> Quiet then update_dep ad else ()

let get_log_level ad =
	try
		ADMap.find ad !ad_log_level
	with | Not_found -> !global_log_level

let set_global string_level =
	global_log_level := (string_to_log_level string_level)

let log_ad string_level string_ad = 
	set_log_level (string_to_ad string_ad) (string_to_log_level string_level)
