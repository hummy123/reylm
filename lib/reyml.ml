module Drawable_types = Drawable_types
module Drawable_sizes = Drawable_sizes
module Drawable_drawing = Drawable_drawing
module Fluent = Fluent
open Drawable_types

let run_app ?(background_col = Raylib.Color.create 243 243 243 255) window_title
    view initial_model =
  let rec loop view (input : 'a draw_widget_input) =
    match Raylib.window_should_close () with
    | true -> Raylib.close_window ()
    | false ->
        let current_view = view input.model in
        let open Raylib in
        begin_drawing ();
        clear_background background_col;
        let output = Drawable_drawing.draw current_view input in
        end_drawing ();
        let input =
          {
            parent_x = 0;
            parent_y = 0;
            parent_w = Raylib.get_screen_width ();
            parent_h = Raylib.get_screen_height ();
            state_tree = output.state_tree;
            model = output.model;
          }
        in
        loop view input
  in
  Raylib.set_config_flags [ Window_resizable; Vsync_hint ];
  Raylib.init_window 0 0 window_title;
  Raylib.maximize_window ();
  let input =
    {
      parent_x = 0;
      parent_y = 0;
      parent_w = Raylib.get_screen_width ();
      parent_h = Raylib.get_screen_height ();
      state_tree = State_tree.SE;
      model = initial_model;
    }
  in
  loop view input
