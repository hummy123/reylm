type progress_state = { pos : float }

let initial_state = { pos = -1.0 }

module type Indeterminate_progress_data = sig
  type key

  val default_foreground : Raylib.Color.t
  val default_background : Raylib.Color.t
  val radius : float
  val direction_size : int
  val anti_direction_size : int
end

module type S = sig
  type key

  val did_change : unit -> bool

  val horizontal :
    key:key ->
    ?width:int ->
    ?height:int ->
    ?radius:float ->
    ?background:Raylib.Color.t ->
    ?foreground:Raylib.Color.t ->
    unit ->
    'a Drawable.drawable

  val vertical :
    key:key ->
    ?width:int ->
    ?height:int ->
    ?radius:float ->
    ?background:Raylib.Color.t ->
    ?foreground:Raylib.Color.t ->
    unit ->
    'a Drawable.drawable
end

module Make (Key : Indeterminate_progress_data) = struct
  type key = Key.key

  (* Boilerplate initialising internal state. *)
  module State_pair = struct
    type key = Key.key
    type value = progress_state
  end

  module Progress_state = State.Make (State_pair)

  let did_change = Progress_state.did_change

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

  (* view_horizontal and view_vertical can be abstracted into a single function,
     but I don't like the if-statement branch and would prefer two nearly-identical short functions. *)
  let view_horizontal key width height radius foreground background =
    let state =
      match Progress_state.find_opt key with
      | Some x -> x
      | None -> initial_state
    in
    let next_state =
      if state.pos >= 1.0 then initial_state
      else { pos = state.pos +. modulate_speed state.pos }
    in
    Progress_state.set key next_state;
    let foreground_width = modulate_size state.pos in
    Rect.widget ~radius ~width ~height ~color:background
      (Align.widget ~x_shift:state.pos
         (Rect.widget ~radius ~width:foreground_width ~height ~color:foreground
            Empty))

  let view_vertical key width height radius foreground background =
    let state =
      match Progress_state.find_opt key with
      | Some x -> x
      | None -> initial_state
    in
    let next_state =
      if state.pos >= 1.0 then initial_state
      else { pos = state.pos +. modulate_speed state.pos }
    in
    Progress_state.set key next_state;
    let foreground_height = modulate_size state.pos in
    Rect.widget ~radius ~width ~height ~color:background
      (Align.widget ~y_shift:(state.pos *. -1.0)
         (Rect.widget ~radius ~width ~height:foreground_height ~color:foreground
            Empty))

  let horizontal ~key ?(width = Key.direction_size)
      ?(height = Key.anti_direction_size) ?(radius = Key.radius)
      ?(background = Key.default_background)
      ?(foreground = Key.default_foreground) () =
    view_horizontal key width height radius foreground background

  let vertical ~key ?(width = Key.anti_direction_size)
      ?(height = Key.direction_size) ?(radius = Key.radius)
      ?(background = Key.default_background)
      ?(foreground = Key.default_foreground) () =
    view_vertical key width height radius foreground background
end
