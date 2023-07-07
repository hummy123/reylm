open Reyml

type dir = Up | Down
type model = { dir : dir; y_pos : float }

let initial = { dir = Down; y_pos = 0.0 }
let k = Key.create ()

let view model =
  Align.widget ~y_shift:model.y_pos
    (Row.left
       [|
         Rect.widget ~width:100 ~height:100 ~color:Raylib.Color.gray Empty;
         Conditional.exec
           (fun _ -> true)
           (fun model ->
             match model.dir with
             | Up ->
                 if model.y_pos >= -1.0 then
                   { model with y_pos = model.y_pos -. 0.01 }
                 else { model with dir = Down }
             | Down ->
                 if model.y_pos <= 1.0 then
                   { model with y_pos = model.y_pos +. 0.01 }
                 else { model with dir = Up });
       |])

let () = run_app view initial
