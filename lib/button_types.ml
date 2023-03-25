type action = Hovering | ClickHeld | ClickReleased | Inactive
type state = { action : action; anim_value : float }

let initial_button_state = { action = Inactive; anim_value = 0.0 }
