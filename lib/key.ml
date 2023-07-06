let total_keys = ref min_int

type t = int

let create () : t =
  let key = !total_keys in
  total_keys := !total_keys + 1;
  key
