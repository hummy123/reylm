open Constraints
open Drawable

let size (constraints : input_constraints) =
  { width = constraints.max_width; height = constraints.max_height }

let draw child (constraints : input_constraints) =
  let child_size = Drawable.size constraints child in
  let max_w = constraints.max_width in
  let max_h = constraints.max_height in
  let x_middle = (max_w / 2) - (child_size.width / 2) in
  let y_middle = (max_h / 2) - (child_size.height / 2) in
  let child_constraints =
    {
      constraints with
      start_x = constraints.start_x + x_middle;
      start_y = constraints.start_y + y_middle;
    }
  in
  let _ = Drawable.draw child_constraints child in
  { width = max_w; height = max_h }

let update child constraints =
  let child_size = Drawable.size (drop_model constraints) child in
  let max_w = constraints.max_width in
  let max_h = constraints.max_height in
  let x_middle = (max_w / 2) - (child_size.width / 2) in
  let y_middle = (max_h / 2) - (child_size.height / 2) in
  let child_constraints =
    {
      constraints with
      start_x = constraints.start_x + x_middle;
      start_y = constraints.start_y + y_middle;
    }
  in
  let { model; _ } = Drawable.update_model child_constraints child in
  { width = max_w; height = max_h; model }

let widget child = Widget (draw child, size, update child)
