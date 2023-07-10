open Constraints
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

let draw width height child constraints =
  let ({ width; height } as w_size : drawable_size) =
    size width height constraints
  in
  let child_constraints =
    { constraints with max_height = height; max_width = width }
  in
  let _ = draw child_constraints child in
  w_size

let update width height child constraints model =
  let ({ width; height } : drawable_size) = size width height constraints in
  let child_constraints =
    { constraints with max_height = height; max_width = width }
  in
  let { model; _ } = Drawable.update child_constraints model child in
  { width; height; model }

let widget ~width ~height child =
  Widget (draw width height child, size width height, update width height child)
