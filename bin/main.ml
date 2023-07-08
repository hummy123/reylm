open Reyml
open Raylib

type model = { color : Color.t }

let initial_model = { color = Color.create 0 0 0 0 }

let update model =
  let rgb_val = Raylib.Color.r model.color in
  let rgb_val = rgb_val + 1 in
  let color = Color.create rgb_val rgb_val rgb_val rgb_val in
  { color }

let view model =
  Percent_rect.widget ~width:0.5 ~height:0.5
    ~color:(Raylib.Color.create 0 0 0 255)
    Empty

let () = run_app view initial_model
