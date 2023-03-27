open Reyml
open Reyml.Drawable

let placeholder =
  Align
    ( Middle,
      Middle,
      Row
        [
          Padding
            ( 10,
              10,
              10,
              10,
              Other
                (Fluent.button "first fluent button", Fluent.button_size, Empty)
            );
          Column
            [
              Rect
                ( 81,
                  41,
                  0.3,
                  Raylib.Color.create 0 0 0 0,
                  Rect (80, 40, 0.2, Raylib.Color.create 0 0 0 255, Empty) );
            ];
          Row
            [
              Column
                [
                  Align
                    (Top, Left, Rect (100, 100, 0.3, Raylib.Color.red, Empty));
                  Align
                    (Middle, Left, Rect (100, 100, 0.3, Raylib.Color.red, Empty));
                  Align
                    (Bottom, Left, Rect (100, 100, 0.3, Raylib.Color.red, Empty));
                ];
              Column
                [
                  Align
                    (Top, Middle, Rect (100, 100, 0.3, Raylib.Color.red, Empty));
                  Align
                    ( Middle,
                      Middle,
                      Rect (100, 100, 0.3, Raylib.Color.red, Empty) );
                  Align
                    ( Bottom,
                      Middle,
                      Rect (100, 100, 0.3, Raylib.Color.red, Empty) );
                ];
              Column
                [
                  Align
                    (Top, Right, Rect (100, 100, 0.3, Raylib.Color.red, Empty));
                  Align
                    ( Middle,
                      Right,
                      Rect (100, 100, 0.3, Raylib.Color.red, Empty) );
                  Align
                    ( Bottom,
                      Right,
                      Rect (100, 100, 0.3, Raylib.Color.red, Empty) );
                ];
            ];
        ] )

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
      let state = Reyml.Drawable.draw placeholder state in
      draw_text "Congrats! You created your first window!" 190 200 20
        Color.lightgray;
      end_drawing ();
      loop state

let () =
  let _ = setup () in
  loop Reyml.initial_state
