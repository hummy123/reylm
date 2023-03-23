type drawable = Column of drawable list | Rectangle of int * int

let placeholder = Column [ Rectangle (200, 200); Rectangle (200, 200) ]

let rec calc available_height available_width = function
  | Rectangle (w, h) ->
      let w = if w < available_width then w else available_width in
      let h = if h < available_height then h else available_height in
      (w, h)
  | Column lst ->
      let length = List.length lst in
      let max_w = available_width / length in
      let max_h = available_height / length in
      let _ =
        List.fold_left
          (fun y_pos el ->
            let w, h = calc max_h max_w el in
            Raylib.draw_rectangle 0 y_pos h w Raylib.Color.black;
            y_pos + h)
          0 lst
      in
      (max_w, max_h)

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
      let _ = calc height width placeholder in
      clear_background Color.raywhite;
      draw_text "Congrats! You created your first window!" 190 200 20
        Color.lightgray;
      end_drawing ();
      loop ()

let () = setup () |> loop
