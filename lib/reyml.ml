module Drawable = Drawable
module Fluent = Fluent

let run_app view initial_model =
  let rec loop view (model : 'a) state =
    match Raylib.window_should_close () with
    | true -> Raylib.close_window ()
    | false ->
        let current_view = view model in
        let open Raylib in
        begin_drawing ();
        clear_background (Color.create 243 243 243 255);
        let state, model = Drawable.draw model current_view state in
        end_drawing ();
        loop view model state
  in
  Raylib.set_config_flags [ Window_resizable; Vsync_hint ];
  Raylib.init_window 0 0 "raylib [core] example - basic window";
  Raylib.maximize_window ();
  loop view initial_model State_tree.SE
