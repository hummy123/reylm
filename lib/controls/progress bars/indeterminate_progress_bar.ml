let total_keys = ref min_int

type linear_state = { pos : float }

let initial_state = { pos = -1.0 }

module Progress_key = struct
  type key = int
  type value = linear_state
end

module Progress_state = State.Make (Progress_key)

let set k v = Progress_state.set k v

let key () =
  let key = !total_keys in
  total_keys := !total_keys + 1;
  key

(* I don't 100% know what I am doing with modulate functions
   but trying to make it look nice and it does. *)
let modulate_size pos =
  let pos = if pos < 0. then pos *. -1.0 else pos in
  let pos = pos -. 1.0 in
  let pos = if pos < 0. then pos *. -1.0 else pos in
  let pos = Easing.ease_in_quad pos *. 75.0 in
  if pos <= 10. then pos +. 10. |> int_of_float else pos |> int_of_float

let modulate_speed pos =
  let pos = if pos < 0. then pos *. -1.0 else pos in
  let pos = pos -. 1.0 in
  let pos = if pos < 0. then pos *. -1.0 else pos in
  let pos = Easing.ease_in_quint pos *. 0.03 in
  if pos < 0.02 then 0.02 else pos

type axis = Horizontal | Vertical

(* view_horizontal and view_vertical can be abstracted into a single function,
   but I don't like the if-statement branch and would prefer two nearly-identical short functions. *)
let view_horizontal key width height radius foreground_col background_col =
  let state =
    match Progress_state.find_opt key with Some x -> x | None -> initial_state
  in
  let next_state =
    if state.pos >= 1.0 then initial_state
    else { pos = state.pos +. modulate_speed state.pos }
  in
  Progress_state.set key next_state;
  let foreground_width = modulate_size state.pos in
  Rect.widget ~radius ~width ~height ~color:background_col
    (Align.widget ~x_shift:state.pos
       (Rect.widget ~radius ~width:foreground_width ~height
          ~color:foreground_col Empty))

let view_vertical key width height radius foreground_col background_col =
  let state =
    match Progress_state.find_opt key with Some x -> x | None -> initial_state
  in
  let next_state =
    if state.pos >= 1.0 then initial_state
    else { pos = state.pos +. modulate_speed state.pos }
  in
  Progress_state.set key next_state;
  let foreground_height = modulate_size state.pos in
  Rect.widget ~radius ~width ~height ~color:background_col
    (Align.widget ~y_shift:(state.pos *. -1.0)
       (Rect.widget ~radius ~width ~height:foreground_height
          ~color:foreground_col Empty))

let default_bg = Raylib.Color.create 214 214 214 255
let default_fg = Raylib.Color.create 0 102 180 255

let horizontal ?(width = max_int) ?(height = 5) ?(radius = 1.0)
    ?(background_col = default_bg) ?(foreground_col = default_fg) key =
  view_horizontal key width height radius foreground_col background_col

let vertical ?(width = 5) ?(height = max_int) ?(radius = 1.0)
    ?(background_col = default_bg) ?(foreground_col = default_fg) key =
  view_vertical key width height radius foreground_col background_col
