open Reyml

let fluent_shadow parent_x parent_y parent_w parent_h =
  Raylib.draw_line parent_x (parent_y + parent_h) (parent_x + parent_w)
    (parent_y + parent_h)
    (Raylib.Color.create 0 0 0 2);
  Raylib.draw_line (parent_x + 1)
    (parent_y + parent_h - 1)
    (parent_x + parent_w - 1)
    (parent_y + parent_h - 1)
    (Raylib.Color.create 0 0 0 4);
  Raylib.draw_line (parent_x + 2) (parent_y + parent_h)
    (parent_x + parent_w - 2)
    (parent_y + parent_h)
    (Raylib.Color.create 0 0 0 8);
  Raylib.draw_line (parent_x + 4)
    (parent_y + parent_h + 1)
    (parent_x + parent_w - 4)
    (parent_y + parent_h + 1)
    (Raylib.Color.create 0 0 0 16);
  (parent_w, parent_h)

let fluent_button _ _ width height =
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
              Raylib.Color.create 251 251 251 255,
              Other (fluent_shadow, Empty) ) ) )

let placeholder =
  Row
    [
      Padding (10, 10, 10, 10, fluent_button () () 160 36);
      Padding (10, 10, 10, 10, fluent_button () () 320 64);
      Padding
        ( 20,
          20,
          20,
          20,
          Rect
            ( 81,
              41,
              0.3,
              Raylib.Color.create 0 0 0 10,
              Rect (80, 40, 0.2, Raylib.Color.create 246 246 246 255, Empty) )
        );
      Column
        [
          Border
            ( 10.0,
              Raylib.Color.raywhite,
              4.0,
              Rect (50, 300, 0.4, Raylib.Color.darkgreen, Empty) );
          Border
            ( 0.2,
              Raylib.Color.pink,
              1.0,
              Rect
                ( 200,
                  200,
                  0.2,
                  Raylib.Color.orange,
                  Other (fluent_shadow, Empty) ) );
        ];
      Padding
        ( 50,
          50,
          50,
          50,
          Rect (80, 60, 0.1, Raylib.Color.create 246 246 246 255, Empty) );
    ]

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
