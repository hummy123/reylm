open Reyml
open Reyml.Drawable_types

type model = { counter : int }

let initial_model = { counter = 0 }

let placeholder _ =
  ColumnCenter [
  RowCenter
    [
      Rect (100, 100, 0.3, Raylib.Color.red, Empty);
      Rect (100, 100, 0.3, Raylib.Color.red, Empty);
      Rect (100, 100, 0.3, Raylib.Color.red, Empty);
      Rect (100, 100, 0.3, Raylib.Color.red, Empty);
      Rect (100, 100, 0.3, Raylib.Color.red, Empty);
    ]
  ]

let () = Reyml.run_app "Test" placeholder initial_model
