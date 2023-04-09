open Reyml

let view = Padding.widget ~left:10 ~top:30 (Rect.widget ~width:60 ~height:30 ())
let () = run_app view
