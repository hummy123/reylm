open Drawable_types

let widget name (input : 'a draw_widget_input) =
  (* Manage state. *)
  let state =
    match State_tree.find_opt name input.state_tree with
    | Some (ListView x) -> x
    | _ -> Listview_state.initial
  in
  let scroll = Raylib.get_mouse_wheel_move_v () in
  let scroll_x = Raylib.Vector2.x scroll in
  let scroll_y = Raylib.Vector2.y scroll in

  let friction =
    if scroll_x <> 0.0 || scroll_y <> 0.0 then 1.0
    else if state.friction >= 0.075 then state.friction -. 0.075
    else 0.0
  in
  let open Listview_state in
  let h_dir =
    if scroll_x = 0.0 then if friction > 0.0 then state.h_dir else 0.0
    else scroll_x
  in
  let v_dir =
    if scroll_y = 0.0 then if friction > 0.0 then state.v_dir else 0.0
    else scroll_y
  in
  let state = { state with friction; h_dir; v_dir } in

  let friction = Easing.ease_out_cubic friction *. 8. in
  let scroll_y =
    if state.friction > 0.0 then
      if v_dir = 1.0 then scroll_y +. friction
      else if v_dir = -1.0 then scroll_y -. friction
      else scroll_y
    else scroll_y
  in

  let scroll_x =
    if state.friction > 0.0 then
      if h_dir = 1.0 then scroll_x +. friction
      else if h_dir = -1.0 then scroll_x -. friction
      else scroll_x
    else scroll_x
  in

  let start_x = state.start_x + int_of_float scroll_x in
  let start_y = state.start_y + int_of_float scroll_y in
  let state = { state with start_x; start_y } in
  (* Unload previous texture. *)
  let _ =
    match state.texture with Some x -> Raylib.unload_texture x | None -> ()
  in
  let str =
    "wertyu\n\
    \    [ok[pk[\n\
    \      pl[pp[kp[k[p\n\
    \      k[p\n\
    \      k[pk[\n\
    \        pk[\n\
    \          pk[\n\
    \            pk[\n\
    \              k[\n\
    \                k[pk[\n\
    \                  pk[pki"
  in
  let img = Raylib.image_text str 20 Raylib.Color.raywhite in
  let rect =
    Raylib.Rectangle.create (float_of_int start_x) (float_of_int start_y)
      (float_of_int input.parent_w)
      (float_of_int input.parent_h)
  in
  Raylib.image_crop (Raylib.addr img) rect;
  let texture = Raylib.load_texture_from_image img in
  (* Unload image and cache new texture. *)
  Raylib.unload_image img;
  let cached_texture = Some texture in
  let state = { state with texture = cached_texture } in
  let state_tree = State_tree.add name (ListView state) input.state_tree in
  (* Draw texture and then return. *)
  Raylib.draw_texture texture input.parent_x input.parent_y Raylib.Color.red;
  {
    width = input.parent_w;
    height = input.parent_h;
    state_tree;
    model = input.model;
  }
