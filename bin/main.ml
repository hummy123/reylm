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

let view model =
  Row.space_around
    [|
      Align.widget ~x_shift:model.y_pos
        (Rect.widget ~width:100 ~height:100 ~color:Raylib.Color.gray
           (Conditional.exec (fun _ -> true) (fun model -> animate model)));
    |]

let () = run_app view initial
