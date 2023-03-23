type drawable = Column of drawable list | Rectangle of int * int

let placeholder = Column [ Rectangle (20, 200); Rectangle (100, 400) ]

let rec calc parent_w parent_h = function
  | Rectangle (w, h) ->
      let w = if w < parent_w then w else parent_w in
      let h = if h < parent_h then h else parent_h in
      (w, h)
  | Column lst ->
      let length = List.length lst in
      let max_w = parent_w / length in
      let max_h = parent_h / length in
      let _ =
        List.fold_left
          (fun y_pos el ->
            let w, h = calc max_h max_w el in
            Raylib.draw_rectangle 0 y_pos w h Raylib.Color.black;
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
