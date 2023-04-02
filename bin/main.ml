open Reyml
open Reyml.Drawable_types

type model = { counter : int }

let initial_model = { counter = 0 }

let placeholder model =
      RowCenter [
        Fluent.text (Format.sprintf "Clicked %i times!" model.counter);
      ]

let () = Reyml.run_app "Test" placeholder initial_model
