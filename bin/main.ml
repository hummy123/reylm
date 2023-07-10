open Reyml
open Reyml.Layout
open Reyml.Controls.Default
open Reyml.Controls.Templates

module Red_template = struct
  type key = int

  let default_background = Raylib.Color.create 214 214 214 255
  let default_foreground = Raylib.Color.create 255 67 67 255
  let radius = 1.0
  let direction_size = max_int
  let anti_direction_size = 10
end

module Red_bar = Indeterminate_progress_bars.Make (Red_template)

let _ = register [| Red_bar.did_change |]

type model = { red_delay : int }

let initial_model = { red_delay = 0 }
let update model = { red_delay = model.red_delay + 1 }

let view model =
  Column.space_around
    [|
      Indeterminate_progress_bar.horizontal ~key:"asdf" ();
      (if model.red_delay > 10 then Red_bar.horizontal ~key:1 () else Empty);
      Conditional.exec true update;
      Indeterminate_progress_bar.horizontal ~key:"awer" ();
    |]

let () = run_app view initial_model
