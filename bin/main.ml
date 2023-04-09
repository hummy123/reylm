open Reyml

let view = Align.widget ~x_shift:0.1 (Rect.widget ~width:60 ~height:30 ())
let () = run_app view
