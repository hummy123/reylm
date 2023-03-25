let power num by_power =
  let rec pow counter acc =
    if counter = 0.0 then acc else pow (counter -. 1.0) (acc *. num)
  in
  pow (by_power -. 1.0) num

let ease_in_circ x =
  let x = 1.0 -. power x 2.0 in
  1.0 -. sqrt x

let ease_out_circ x =
  let x = power (x -. 1.0) 2.0 in
  let x = 1.0 -. x in
  sqrt x

let ease_in_col src dst x =
  let r = Raylib.Color.r dst in
  let g = Raylib.Color.g dst in
  let b = Raylib.Color.b dst in
  let anim_value = int_of_float (ease_in_circ x *. 255.0) in
  let tint = Raylib.Color.create r g b anim_value in
  Raylib.color_alpha_blend src dst tint

let ease_out_col src dst x =
  let r = Raylib.Color.r dst in
  let g = Raylib.Color.g dst in
  let b = Raylib.Color.b dst in
  let anim_value = int_of_float (ease_out_circ x *. 255.0) in
  let tint = Raylib.Color.create r g b anim_value in
  Raylib.color_alpha_blend src dst tint
