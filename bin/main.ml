open Reyml

let placeholder =
  Row
    [
      Box
        ( Raylib.Color.black,
          Column
            [
              Button ("", 20, 200, 0.0, Raylib.Color.red, Empty);
              Padding
                ( 10,
                  20,
                  30,
                  40,
                  Button ("", 100, 400, 0.2, Raylib.Color.beige, Empty) );
            ] );
      Column
        [
          Border
            ( 10.0,
              Raylib.Color.raywhite,
              4.0,
              Button ("", 50, 300, 0.4, Raylib.Color.darkgreen, Empty) );
          Border
            ( 0.2,
              Raylib.Color.pink,
              1.0,
              Button ("", 200, 200, 0.2, Raylib.Color.orange, Empty) );
        ];
    ]

let setup () =
  let width = Raylib.get_monitor_width 0 in
  let height = Raylib.get_monitor_height 0 in
  Raylib.set_config_flags [ Window_resizable; Vsync_hint ];
  Raylib.init_window width height "raylib [core] example - basic window";
  ()

let rec loop () =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let height = Raylib.get_screen_height () in
      let width = Raylib.get_screen_width () in
      let open Raylib in
      begin_drawing ();
      let _ = Reyml.draw 0 0 width height placeholder in
      clear_background Color.raywhite;
      draw_text "Congrats! You created your first window!" 190 200 20
        Color.lightgray;
      end_drawing ();
      loop ()

let () = setup () |> loop
