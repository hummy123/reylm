open Raylib
open Drawable

let size width height constraints =
  let width =
    if constraints.min_width > width then constraints.min_width
    else if constraints.max_width < width then constraints.max_width
    else width
  in
  let height =
    if constraints.min_height > height then constraints.min_height
    else if constraints.max_height < height then constraints.max_height
    else height
  in
  { width; height }

let draw width height radius color child constraints =
  let ({ width; height } as w_size) = size width height constraints in
  let f_width = float_of_int width in
  let f_height = float_of_int height in
  let x = float_of_int constraints.start_x in
  let y = float_of_int constraints.start_y in
  let rect = Rectangle.create x y f_width f_height in
  Raylib.draw_rectangle_rounded rect radius 0 color;
  let child_constraints =
    { constraints with max_height = height; max_width = width }
  in
  let _ = draw child_constraints child in
  w_size

let widget ?(radius = 0.0) ?(color = Color.black) ~width ~height child =
  Widget (draw width height radius color child, size width height)
