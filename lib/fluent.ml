open Drawable

let fluent_button ?(width = 160) ?(height = 32) parent_x parent_y _ _ =
  let light parent_x parent_y parent_w parent_h ~lightest_alpha =
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
    (parent_w, parent_h)
  in

  let shadow parent_x parent_y parent_w parent_h ~darkest_alpha =
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
    (parent_w, parent_h)
  in

  let mouse = Raylib.get_mouse_position () in
  let mouse_x = int_of_float (Raylib.Vector2.x mouse) in
  let mouse_y = int_of_float (Raylib.Vector2.y mouse) in
  let is_hovering =
    (parent_x <= mouse_x && parent_x + width >= mouse_x)
    && parent_y <= mouse_y
    && parent_y + height >= mouse_y
  in
  let did_click = Raylib.is_mouse_button_down Raylib.MouseButton.Left in
  let col, light_alpha, dark_alpha =
    if is_hovering && did_click then (Raylib.Color.create 251 251 251 128, 8, 2)
    else if is_hovering then (Raylib.Color.create 251 251 251 192, 32, 8)
    else (Raylib.Color.create 251 251 251 255, 48, 12)
  in

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
  let _ = draw_widget parent_x parent_y width height view in
  (width, height)
