open Button_types

let reduce state action =
  match action with
  | Hovering ->
      let anim_value = state.anim_value +. 0.05 in
      let anim_value = if anim_value >= 1.0 then 1.0 else anim_value in
      { action = Hovering; anim_value }
  | ClickHeld ->
      let anim_value =
        if state.action <> ClickHeld then 0.0 else state.anim_value +. 0.05
      in
      let anim_value = if anim_value >= 1.0 then 1.0 else anim_value in
      { action = ClickHeld; anim_value }
  | Inactive ->
      let anim_value = state.anim_value -. 0.05 in
      let anim_value = if anim_value <= 0.0 then 0.0 else anim_value in
      { state with anim_value }

let get_hover_col c_unselected c_hover (state : state) =
  let r = Raylib.Color.r c_hover in
  let g = Raylib.Color.g c_hover in
  let b = Raylib.Color.b c_hover in
  let anim_value =
    int_of_float (Easing.ease_out_circ state.anim_value *. 255.0)
  in
  let tint = Raylib.Color.create r g b anim_value in
  Raylib.color_alpha_blend c_unselected c_unselected tint
