open Drawable_types
open Drawable_drawing

let get_col (state : Button_state.button_state) col =
  let base_col =
    Raylib.Color.create (Raylib.Color.r col) (Raylib.Color.g col)
      (Raylib.Color.b col)
  in
  match state.action with
  | Button_state.Inactive -> base_col 255
  | Button_state.Hover ->
      let anim_value =
        int_of_float (Easing.ease_in_circ state.easing *. 255.0)
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
  | Button_state.Hover -> int_of_float (Easing.ease_in_circ state.easing *. 48.0)
  | Button_state.ClickHeld ->
      int_of_float (Easing.ease_in_cubic state.easing *. 12.0)

let get_dark_alpha (state : Button_state.button_state) =
  match state.action with
  | Button_state.Inactive -> 16
  | Button_state.Hover -> int_of_float (Easing.ease_in_circ state.easing *. 12.)
  | Button_state.ClickHeld ->
      int_of_float (Easing.ease_in_circ state.easing *. 4.)

let widget name ?(text = "") ?width ?(height = 32) ?(on_click = fun x -> x)
    ?(background_color = Raylib.Color.create 251 251 251 255)
    ?(text_color = Raylib.Color.create 32 28 28 255) parent_x parent_y parent_w
    parent_h (state_tree : State_tree.state_tree) model =
  let child_width =
    let width, _ = Fluent_text.size text 0 0 0 in
    width + 40
  in
  let width =
    match width with
    | Some x -> if x > child_width then x else child_width
    | None -> child_width
  in

  (* Colours for light/dark. *)
  let r = Raylib.Color.r background_color in
  let r_light = if r + 20 >= 255 then 255 else r + 20 in
  let r_dark = if r - 80 <= 0 then 0 else r - 80 in
  let g = Raylib.Color.g background_color in
  let g_light = if g + 20 >= 255 then 255 else g + 20 in
  let g_dark = if g - 80 <= 0 then 0 else g - 80 in
  let b = Raylib.Color.b background_color in
  let b_light = if b + 20 >= 255 then 255 else b + 20 in
  let b_dark = if b - 80 <= 0 then 0 else b - 80 in

  (* Bools for indicating whether events occured. *)
  let mouse = Raylib.get_mouse_position () in
  let mouse_x = int_of_float (Raylib.Vector2.x mouse) in
  let mouse_y = int_of_float (Raylib.Vector2.y mouse) in
  let is_hovering =
    (parent_x <= mouse_x && parent_x + width >= mouse_x)
    && parent_y <= mouse_y
    && parent_y + height >= mouse_y
  in
  let did_click = Raylib.is_mouse_button_pressed Raylib.MouseButton.Left in
  let click_held = Raylib.is_mouse_button_down Raylib.MouseButton.Left in

  (* Manage button state. *)
  let reduce state =
    if is_hovering && click_held then Button_state.reduce state ClickHeld
    else if is_hovering then Button_state.reduce state Hover
    else Button_state.reduce state Inactive
  in

  let state =
    match State_tree.find_opt name state_tree with
    | Some (Button x) -> reduce x
    | _ -> reduce Button_state.initial
  in
  let state_tree = State_tree.add name (Button state) state_tree in

  let model =
    if (Button_state.did_click state || did_click) && is_hovering then
      on_click model
    else model
  in

  (* Draw button. *)
  let button_col = get_col state background_color in
  let base_light = Raylib.Color.create r_light g_light b_light in
  let light_alpha = get_light_alpha state in
  let light1 = base_light light_alpha in
  let light2 = base_light (light_alpha / 2) in
  let light3 = base_light (light_alpha / 4) in
  let light4 = base_light (light_alpha / 8) in
  let light5 = base_light (light_alpha / 16) in
  let light6 = base_light (light_alpha / 32) in

  let base_dark = Raylib.Color.create r_dark g_dark b_dark in
  let dark_alpha = get_dark_alpha state in
  let dark1 = base_dark (dark_alpha / 8) in
  let dark2 = base_dark (dark_alpha / 4) in
  let dark3 = base_dark (dark_alpha / 2) in
  let dark4 = base_dark dark_alpha in

  let text_color = get_col state text_color in

  let view =
    Rect
      ( width,
        height,
        0.2,
        Raylib.Color.create 0 0 0 16,
        Overlay
          [
            (* Main button area. *)
            Padding (1, 1, 1, 2, Rect (width, height, 0.2, button_col, Empty));
            (* Top and bottom shadows. *)
            ColumnSpaceBetween
              [
                ColumnStart
                  [
                    Padding (3, 0, 3, 0, HLine light1);
                    Padding (2, 0, 2, 0, HLine light2);
                    Padding (1, 0, 1, 0, HLine light3);
                    HLine light4;
                    HLine light5;
                    HLine light6;
                  ];
                RowCenter
                  [
                    Other
                      ( Fluent_text.widget text ~col:text_color
                          ~font_path:Fonts.light,
                        Fluent_text.size text,
                        Empty );
                  ];
                ColumnEnd
                  [
                    HLine dark1;
                    Padding (1, 0, 1, 0, HLine dark2);
                    Padding (2, 0, 2, 0, HLine dark3);
                    Padding (3, 0, 3, 0, HLine dark4);
                  ];
              ];
          ] )
  in
  let text_w, text_h, state_tree, model =
    draw_widget parent_x parent_y width height state_tree model view
  in
  (width, height, state_tree, model)

let size parent_w parent_h _ = (parent_w, parent_h)
