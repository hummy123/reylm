open Raylib
open Constraints
open Drawable

(* Functions and data related to managing state of controls. *)
let widget_tables =
  ref [| Indeterminate_progress_bar.Progress_state.did_change |]

let register_state state_array =
  let arr = Array.append !widget_tables state_array in
  widget_tables := arr

let rec did_widgets_change ?(value = false) pos =
  if value then true
  else if pos < 0 then value
  else
    let f = Array.unsafe_get !widget_tables pos in
    let is_true = f () in
    did_widgets_change ~value:(value || is_true) (pos - 1)

let should_redraw old_model new_model =
  if old_model == new_model then
    did_widgets_change (Array.length !widget_tables - 1)
  else true

(* Functions and data related to app loop. *)
let default_bg = Raylib.Color.create 243 243 243 255
let default_title = "default_title"

let rec app_loop view model =
  match
    (Raylib.window_should_close (), Raylib.is_key_pressed Raylib.Key.Escape)
  with
  | true, false -> Raylib.close_window ()
  | _ ->
      let cur_view = view model in
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
      clear_background default_bg;
      let _ = Drawable.draw constraints_from_root cur_view in
      let { model = new_model; _ } =
        Drawable.update constraints_from_root model cur_view
      in
      end_drawing ();
      if should_redraw model new_model then Raylib.disable_event_waiting ()
      else Raylib.enable_event_waiting ();
      app_loop view new_model

let run_app ?(window_title = default_title) view initial_model =
  Raylib.set_config_flags [ Window_maximized; Window_resizable; Vsync_hint ];
  let width = 1600 in
  let height = 900 in
  Raylib.init_window width height window_title;
  Raylib.enable_event_waiting ();
  app_loop view initial_model
