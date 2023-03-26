type button_action = Hover | ClickHeld | Inactive
type button_state = { action : button_action; easing : float }

let initial = { action = Inactive; easing = 0.9 }
let initial_easing = 0.9
let max_hover_easing = 1.0
let min_clicked_easing = 0.8

let reduce state = function
  | Hover ->
      let easing = if state.easing >= 1.0 then 1.0 else state.easing +. 0.01 in
      { action = Hover; easing }
  | ClickHeld ->
      let easing =
        if state.easing <= 0.85 then 0.85 else state.easing -. 0.01
      in
      { action = ClickHeld; easing }
  | Inactive -> initial
