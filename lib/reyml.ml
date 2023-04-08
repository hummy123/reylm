module Drawable = Drawable
open Drawable

let default_bg = Raylib.Color.create 243 243 243 255
let default_title = "default_title"

let run_app ?(background_col = default_bg) ?(window_title = default_title) view
    =
  let rec loop view =
    match Raylib.window_should_close () with
    | true ->
        if Raylib.is_key_pressed Raylib.Key.Escape then loop view
        else Raylib.close_window ()
    | false ->
        let open Raylib in
        let constraints_from_root =
          {
            start_x = 0;
            start_y = 0;
            min_width = 0;
            min_height = 0;
            max_width = Raylib.get_screen_width ();
            max_height = Raylib.get_screen_height ();
          }
        in
        begin_drawing ();
        clear_background background_col;
        let _ = Drawable.draw constraints_from_root view in
        end_drawing ();
        loop view
  in
  Raylib.set_config_flags [ Window_maximized; Window_resizable; Vsync_hint ];
  Raylib.init_window 0 0 window_title;
  loop view
