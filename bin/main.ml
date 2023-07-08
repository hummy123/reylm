open Reyml

type dir = Up | Down
type model = { dir : dir; y_pos : float }

let initial = { dir = Down; y_pos = 0.0 }

let animate model =
  match model.dir with
  | Up ->
      if model.y_pos >= -1.0 then { model with y_pos = model.y_pos -. 0.015 }
      else { model with dir = Down }
  | Down ->
      if model.y_pos <= 1.0 then { model with y_pos = model.y_pos +. 0.015 }
      else { model with dir = Up }

let key = Progress_bar.key ()

let view model =
  Center.widget (Padding.by_axis ~horizontal:100 (Progress_bar.widget key))

let () = run_app view initial
