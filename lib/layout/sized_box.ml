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
  let ({ width; height } : drawable_size) = size width height constraints in
  let child_constraints =
    { constraints with max_height = width; max_width = height }
  in
  let _ = draw child_constraints child in
  size width height constraints

let update width height child constraints model =
  let ({ width; height } : drawable_size) = size width height constraints in
  let child_constraints =
    { constraints with max_height = width; max_width = height }
  in
  let { model; _ } = update child_constraints model child in
  { width; height; model }

let widget ~width ~height child =
  Widget (draw width height child, size width height, update width height child)
