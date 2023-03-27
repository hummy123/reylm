type button_action = Hover | ClickHeld | Inactive
type button_state = { action : button_action; easing : float }

let initial = { action = Inactive; easing = 1.0 }

let reduce state = function
  | Hover ->
      let easing = if state.easing <= 0.9 then 0.9 else state.easing -. 0.01 in
      { action = Hover; easing }
  | ClickHeld ->
      let easing = if state.easing <= 0.8 then 0.8 else state.easing -. 0.01 in
      { action = ClickHeld; easing }
  | Inactive ->
      if state.easing >= 1.0 then initial
      else { state with easing = state.easing +. 0.01 }
