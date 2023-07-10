(* Hash table for keeping track of widget state.
   Can pass around immutable data structure for same effect,
   but don't need previous versions and Hashtbl is faster. *)
module type State_pair = sig
  type key
  type value
end

module Make (Key : State_pair) = struct
  type t = { mutable did_change : bool; tbl : (Key.key, Key.value) Hashtbl.t }

  let state = { did_change = false; tbl = Hashtbl.create 1024 }

  let did_change () =
    let did_change = state.did_change in
    state.did_change <- false;
    did_change

  let find_opt k = Hashtbl.find_opt state.tbl k

  let set key value =
    match Hashtbl.find_opt state.tbl key with
    | Some x ->
        if value = x then () else Hashtbl.add state.tbl key value;
        state.did_change <- true
    | None ->
        Hashtbl.add state.tbl key value;
        state.did_change <- true
end
