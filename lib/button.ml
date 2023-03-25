type action = Hovering | ClickHeld | ClickReleased | Inactive
type state = { action : action; anim_value : float }

let initial_button_state = { action = Inactive; anim_value = 0.0 }

let get_hover_col c_unselected c_hover state =
  let _ = state in
  Raylib.color_alpha_blend c_unselected c_unselected
    (Raylib.Color.create 50 50 50 100)
