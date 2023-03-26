open Drawable

let get_col (state : Button_state.button_state) =
  let base_col = Raylib.Color.create 255 255 255 in
  match state.action with
  | Button_state.Inactive -> base_col 255
  | Button_state.Hover ->
      let anim_value =
        int_of_float (Easing.ease_in_cubic state.easing *. 255.0)
      in
      base_col anim_value
  | Button_state.ClickHeld ->
      let anim_value =
        int_of_float (Easing.ease_in_cubic state.easing *. 255.0)
      in
      base_col anim_value

let get_light_alpha (state : Button_state.button_state) =
  match state.action with
  | Button_state.Inactive -> 64
  | Button_state.Hover ->
      int_of_float (Easing.ease_in_cubic state.easing *. 48.0)
  | Button_state.ClickHeld ->
      int_of_float (Easing.ease_in_cubic state.easing *. 12.0)

let get_dark_alpha (state : Button_state.button_state) =
  match state.action with
  | Button_state.Inactive -> 16
  | Button_state.Hover -> int_of_float (Easing.ease_in_cubic state.easing *. 12.)
  | Button_state.ClickHeld ->
      int_of_float (Easing.ease_in_cubic state.easing *. 4.)

let button name ?(width = 160) ?(height = 32) parent_x parent_y _ _
    (state_tree : State_tree.state_tree) =
  let light ~lightest_alpha parent_x parent_y parent_w parent_h state_tree =
    Raylib.draw_line (parent_x + 3) (parent_y - 1)
      (parent_x + parent_w - 3)
      (parent_y - 1)
      (Raylib.Color.create 255 255 255 lightest_alpha);
    Raylib.draw_line parent_x parent_y (parent_x + parent_w) parent_y
      (Raylib.Color.create 255 255 255 (lightest_alpha / 2));
    Raylib.draw_line parent_x (parent_y + 1) (parent_x + parent_w) (parent_y + 1)
      (Raylib.Color.create 255 255 255 (lightest_alpha / 4));
    Raylib.draw_line parent_x (parent_y + 2) (parent_x + parent_w) (parent_y + 2)
      (Raylib.Color.create 255 255 255 (lightest_alpha / 8));
    Raylib.draw_line parent_x (parent_y + 3) (parent_x + parent_w) (parent_y + 3)
      (Raylib.Color.create 255 255 255 (lightest_alpha / 16));
    Raylib.draw_line parent_x (parent_y + 4) (parent_x + parent_w) (parent_y + 4)
      (Raylib.Color.create 255 255 255 (lightest_alpha / 32));
    (parent_w, parent_h, state_tree)
  in

  let shadow ~darkest_alpha parent_x parent_y parent_w parent_h state_tree =
    Raylib.draw_line parent_x (parent_y + parent_h) (parent_x + parent_w)
      (parent_y + parent_h)
      (Raylib.Color.create 0 0 0 (darkest_alpha / 8));
    Raylib.draw_line (parent_x + 1)
      (parent_y + parent_h - 1)
      (parent_x + parent_w - 1)
      (parent_y + parent_h - 1)
      (Raylib.Color.create 0 0 0 (darkest_alpha / 4));
    Raylib.draw_line (parent_x + 2) (parent_y + parent_h)
      (parent_x + parent_w - 2)
      (parent_y + parent_h)
      (Raylib.Color.create 0 0 0 (darkest_alpha / 2));
    Raylib.draw_line (parent_x + 4)
      (parent_y + parent_h + 1)
      (parent_x + parent_w - 4)
      (parent_y + parent_h + 1)
      (Raylib.Color.create 0 0 0 darkest_alpha);
    (parent_w, parent_h, state_tree)
  in

  let reduce state =
    let mouse = Raylib.get_mouse_position () in
    let mouse_x = int_of_float (Raylib.Vector2.x mouse) in
    let mouse_y = int_of_float (Raylib.Vector2.y mouse) in
    let is_hovering =
      (parent_x <= mouse_x && parent_x + width >= mouse_x)
      && parent_y <= mouse_y
      && parent_y + height >= mouse_y
    in
    let did_click = Raylib.is_mouse_button_down Raylib.MouseButton.Left in
    if is_hovering && did_click then Button_state.reduce state ClickHeld
    else if is_hovering then Button_state.reduce state Hover
    else Button_state.reduce state Inactive
  in

  let state =
    match State_tree.find_opt name state_tree with
    | Some (Button x) -> reduce x
    | _ -> reduce Button_state.initial
  in
  let state_tree = State_tree.add name (Button state) state_tree in

  let col = get_col state in
  let light_alpha = get_light_alpha state in
  let dark_alpha = get_dark_alpha state in

  let view =
    Rect
      ( width,
        height,
        0.2,
        Raylib.Color.create 0 0 0 16,
        Padding
          ( 1,
            1,
            1,
            2,
            Rect
              ( width - 2,
                height - 2,
                0.2,
                col,
                Column
                  [
                    Other (light ~lightest_alpha:light_alpha, Empty);
                    Other (shadow ~darkest_alpha:dark_alpha, Empty);
                  ] ) ) )
  in
  let _, _, state_tree =
    draw_widget parent_x parent_y width height state_tree view
  in
  (width, height, state_tree)
