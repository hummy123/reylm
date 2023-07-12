open Reylm
open Reylm.Layout
open Reylm.Controls.Default
open Reylm.Controls.Templates

(* Create a new Indeterminate_progress_bar type using a functor.*)
module Red_template = struct
  type key = string

  let default_background = Raylib.Color.create 214 214 214 255
  let default_foreground = Raylib.Color.create 255 67 67 255
  let radius = 1.0
  let direction_size = max_int
  let anti_direction_size = 10
end

module Red_bar = Indeterminate_progress_bars.Make (Red_template)

(* Register the bar's internal state with the framework. This is mutable, but that's fine. *)
let _ = register [| Red_bar.did_change |]

(* The model is just a record with an integer, and the update function adds 1 to it. *)
type model = { red_delay : int }

let initial_model = { red_delay = 0 }
let update model = { red_delay = model.red_delay + 1 }

let view model =
  Padding.all 400
    (Column.space_around
       [|
         Indeterminate_progress_bar.horizontal ~key:"asdf" ();
         (* The if statement below displays a red progress bar with the same key but a different state,
            because defining a new functor resets the state. *)
         (if model.red_delay > 10 then Red_bar.horizontal ~key:"asdf" ()
          else Empty);
         (* Conditional.exec just calls a function to update the model whenever some condition is true. *)
         Conditional.exec (model.red_delay <= 10) update;
       |])

let () = run_app view initial_model
