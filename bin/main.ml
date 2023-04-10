open Reyml

let view =
  Align.widget ~x_shift:0.0 ~y_shift:0.0 (Rect.widget ~width:60 ~height:30 ())

let () = run_app view
