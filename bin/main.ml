open Reyml

let fluent_button parent_x parent_y parent_width parent_height ~width ~height =
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
    else (Raylib.Color.create 251 251 251 255, 64, 16)
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
  let w, h = Reyml.draw_control parent_x parent_y width height view in
  (w, h)

let placeholder =
  Row
    [
      Padding
        (10, 10, 10, 10, Other (fluent_button ~width:160 ~height:32, Empty));
    ]
(* Padding *)
(*   ( 20, *)
(*     20, *)
(*     20, *)
(*     20, *)
(*     Rect *)
(*       ( 81, *)
(*         41, *)
(*         0.3, *)
(*         Raylib.Color.create 0 0 0 10, *)
(*         Rect (80, 40, 0.2, Raylib.Color.create 246 246 246 255, Empty) ) *)
(*   ); *)
(* Column *)
(*   [ *)
(*     Border *)
(*       ( 10.0, *)
(*         Raylib.Color.raywhite, *)
(*         4.0, *)
(*         Rect (50, 300, 0.4, Raylib.Color.darkgreen, Empty) ); *)
(*     (* Border *) *)
(*   ( 0.2, *)
(*     Raylib.Color.pink, *)
(*     1.0, *)
(*     Rect *)
(*       ( 200, *)
(*         200, *)
(*         0.2, *)
(*         Raylib.Color.orange, *)
(*         Other (fluent_shadow, Empty) ) ); *)
(* ]; *)
(* Padding *)
(*   ( 50, *)
(*     50, *)
(*     50, *)
(*     50, *)
(*     Rect (80, 60, 0.1, Raylib.Color.create 246 246 246 255, Empty) ); *)
(* ] *)

let setup () =
  let width = Raylib.get_monitor_width 0 in
  let height = Raylib.get_monitor_height 0 in
  Raylib.set_config_flags [ Window_resizable; Vsync_hint ];
  Raylib.init_window width height "raylib [core] example - basic window";
  ()

let rec loop state =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      clear_background (Color.create 243 243 243 255);
      let state = Reyml.draw placeholder state in
      draw_text "Congrats! You created your first window!" 190 200 20
        Color.lightgray;
      end_drawing ();
      loop state

let () =
  let _ = setup () in
  loop ()
