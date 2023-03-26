open Reyml

let col1 _ _ _ =
  Column
    [
      Rect (20, 200, 0.0, Raylib.Color.green, Empty);
      Padding (10, 20, 30, 40, Rect (100, 400, 0.2, Raylib.Color.beige, Empty));
    ]

let placeholder =
  Row
    [
      Box (Raylib.Color.black, col1 "asdf" 2 6);
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
              Rect (200, 200, 0.2, Raylib.Color.orange, Empty) );
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
      clear_background Color.white;
      let state = Reyml.draw placeholder state in
      draw_text "Congrats! You created your first window!" 190 200 20
        Color.lightgray;
      end_drawing ();
      loop state

let () =
  let _ = setup () in
  loop ()
