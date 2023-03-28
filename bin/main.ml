open Reyml
open Reyml.Drawable

type model = { counter : int }

let initial_model = { counter = 0 }

let placeholder model =
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
                ( Fluent.button "first fluent button" ~on_click:(fun model ->
                      { counter = model.counter + 100 }),
                  Fluent.button_size,
                  Align(Middle, Middle, Text((Printf.sprintf "%i" model.counter), 24.0) ) ));
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
  Raylib.set_config_flags [ Window_resizable; Vsync_hint ];
  Raylib.init_window 0 0 "raylib [core] example - basic window";
  Raylib.maximize_window ();
  ()

let rec loop view (model : 'a) state =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let current_view = view model in
      let open Raylib in
      begin_drawing ();
      clear_background (Color.create 243 243 243 255);
      let state, model = Reyml.Drawable.draw model current_view state in
      let font = Raylib.load_font "resources/Noto_Sans/NotoSans-Regular.ttf" in
      let vec = Vector2.create 200. 200. in
      draw_text_ex font "Congrats! You created your first window!" vec 200. 20.
        Color.lightgray;
      end_drawing ();
      loop view model state

let () =
  let _ = setup () in
  loop placeholder initial_model Reyml.initial_state
