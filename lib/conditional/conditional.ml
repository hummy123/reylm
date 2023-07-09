open Constraints
open Drawable

let size_or_draw _ = { width = 0; height = 0 }

let update condition f_update _ model =
  if condition then { width = 0; height = 0; model = f_update model }
  else { width = 0; height = 0; model }

let exec condition f_update =
  Widget (size_or_draw, size_or_draw, update condition f_update)
