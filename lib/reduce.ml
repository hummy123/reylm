type widget_action = ButtonHover of string

let reduce state_tree = function
  | ButtonHover key ->
      let state =
        match State_tree.find_opt key state_tree with
        | Some (ButtonState x) -> x
        | _ -> Button.initial_button_state
      in
      let state : Button.state =
        { action = Hovering; anim_value = state.anim_value + 1 }
      in
      State_tree.add key (ButtonState state) state_tree
