open Reyml

type dir = Left | Right
type model = { dir : dir; left : int }

let initial = { dir = Right; left = 0 }

let view model =
  Padding.from_ltrb ~left:model.left
    (Row.left
       [|
         Rect.widget ~width:100 ~height:100 ~color:Raylib.Color.gray Empty;
         Conditional.exec
           (fun _ -> true)
           (fun model ->
             match model.dir with
             | Left ->
                 if model.left < 1000 then { model with left = model.left + 5 }
                 else { model with dir = Right }
             | Right ->
                 if model.left >= 0 then { model with left = model.left - 5 }
                 else { model with dir = Left });
       |])

let () = run_app view initial
