type widget_action = ButtonHover of string

let reduce state_tree = function
  | ButtonHover key ->
      let state : Button.state = { hovering = true } in
      State_tree.add key (ButtonState state) state_tree
