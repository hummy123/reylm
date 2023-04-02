open Reyml
open Reyml.Drawable_types

type model = { counter : int }

let initial_model = { counter = 0 }

let placeholder model =
  ColumnStart
    [
      Rect (90, 90, 0.0, Raylib.Color.blank, Fluent.listview ());
      Rect (90, 90, 0.2, Raylib.Color.red, Empty);
    ]

let () = Reyml.run_app "Test" placeholder initial_model
