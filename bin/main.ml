open Reyml

let placeholder =
  Row
    [
      Box
        ( Raylib.Color.black,
          Column
            [
              Button
                ( "first",
                  20,
                  200,
                  0.0,
                  Raylib.Color.green,
                  Raylib.Color.red,
                  Raylib.Color.darkgreen,
                  Empty );
              Padding
                ( 10,
                  20,
                  30,
                  40,
                  Button
                    ( "second",
                      100,
                      400,
                      0.2,
                      Raylib.Color.beige,
                      Raylib.Color.orange,
                      Raylib.Color.darkgreen,
                      Empty ) );
            ] );
      Column
        [
          Border
            ( 10.0,
              Raylib.Color.raywhite,
              4.0,
              Button
                ( "third",
                  50,
                  300,
                  0.4,
                  Raylib.Color.darkgreen,
                  Raylib.Color.yellow,
                  Raylib.Color.darkbrown,
                  Empty ) );
          Border
            ( 0.2,
              Raylib.Color.pink,
              1.0,
              Button
                ( "fourth",
                  200,
                  200,
                  0.2,
                  Raylib.Color.orange,
                  Raylib.Color.darkbrown,
                  Raylib.Color.black,
                  Empty ) );
        ];
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
      clear_background Color.raywhite;
      let state = Reyml.draw placeholder state in
      draw_text "Congrats! You created your first window!" 190 200 20
        Color.lightgray;
      end_drawing ();
      loop state

let () =
  let _ = setup () in
  loop Reyml.empty_state
