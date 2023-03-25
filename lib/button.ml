open Button_types

let reduce key state_tree action =
  let state =
    match State_tree.find_opt key state_tree with
    | Some (ButtonState x) -> x
    | _ -> initial_button_state
  in
  match action with
  | Hovering ->
      let anim_value = state.anim_value +. 0.05 in
      let anim_value = if anim_value >= 1.0 then 1.0 else anim_value in
      let state : state = { action = Hovering; anim_value } in
      State_tree.add key (ButtonState state) state_tree
      (* .ClickHeld -> *)
  | _ -> State_tree.add key (ButtonState initial_button_state) state_tree

let get_hover_col c_unselected c_hover (state : state) =
  let r = Raylib.Color.r c_hover in
  let g = Raylib.Color.g c_hover in
  let b = Raylib.Color.b c_hover in
  let anim_value =
    int_of_float (Easing.ease_out_circ state.anim_value *. 255.0)
  in
  let tint = Raylib.Color.create r g b anim_value in
  Raylib.color_alpha_blend c_unselected c_unselected tint
