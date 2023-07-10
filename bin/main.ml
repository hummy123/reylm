open Reyml
open Reyml.Layout
open Reyml.Controls.Default
open Reyml.Controls.Templates

module My_template = struct
  type key = int

  let default_background = Raylib.Color.create 214 214 214 255
  let default_foreground = Raylib.Color.create 255 67 67 255
  let radius = 1.0
  let direction_size = max_int
  let anti_direction_size = 10
end

module My_indeterminate = Indeterminate_progress_bars.Make (My_template)

let _ = register [| My_indeterminate.did_change |]

let view _ =
  Padding.all 100
    (Center.widget
       (Column.space_around
          [|
            My_indeterminate.horizontal ~key:1 ();
            Indeterminate_progress_bar.horizontal ~key:"asdf" ();
          |]))

let () = run_app view ()
