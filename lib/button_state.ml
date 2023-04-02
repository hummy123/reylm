type button_action = Hover | ClickHeld | Inactive
type button_state = { action : button_action; did_click : bool; easing : float }

let initial = { action = Inactive; did_click = false; easing = 1.0 }
let did_click state = state.did_click

let reduce state = function
  | Hover ->
      if state.action <> ClickHeld then
        let easing = state.easing -. 0.01 in
        let easing = if state.easing <= 0.9 then 0.9 else easing in
        { action = Hover; easing; did_click = false }
      else
        let easing = state.easing +. 0.1 in
        let easing = if state.easing >= 0.9 then 0.9 else easing in
        { state with easing; did_click = false }
  | ClickHeld ->
      let did_click = true in
      let easing = if state.easing <= 0.7 then 0.7 else state.easing -. 0.1 in
      { action = ClickHeld; easing; did_click }
  | Inactive ->
      let easing = state.easing +. 1.5 in
      if easing >= 1.0 then initial
      else { state with easing = state.easing +. 0.01; did_click = false }
