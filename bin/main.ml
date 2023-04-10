open Reyml

let view = Flex.force_fill (Rect.widget ~width:60 ~height:30 ())
let () = run_app view
