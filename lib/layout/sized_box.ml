open Constraints
open Drawable

let size width height (constraints : input_constraints) =
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

let draw width height child (constraints : input_constraints) =
  let max_width =
    if width < constraints.min_width then constraints.min_width
    else if width > constraints.max_width then constraints.max_width
    else width
  in
  let max_height =
    if height < constraints.min_height then constraints.min_height
    else if height > constraints.max_height then constraints.max_height
    else height
  in
  let child_constraints = { constraints with max_height; max_width } in
  let _ = draw child_constraints child in
  { height = max_height; width = max_width }

let update width height child constraints =
  let max_width =
    if width < constraints.min_width then constraints.min_width
    else if width > constraints.max_width then constraints.max_width
    else width
  in
  let max_height =
    if height < constraints.min_height then constraints.min_height
    else if height > constraints.max_height then constraints.max_height
    else height
  in
  let child_constraints = { constraints with max_height; max_width } in
  let { model; _ } = update_model child_constraints child in
  { width = max_width; height = max_height; model }

let widget ~width ~height child =
  Widget (draw width height child, size width height, update width height child)
