open Constraints
open Drawable

type alignment_properties = {
  max_w : int;
  max_h : int;
  start_x : int;
  start_y : int;
}

let size (constraints : input_constraints) =
  { width = constraints.max_width; height = constraints.max_height }

let get_properties x_shift y_shift (child_size : drawable_size) start_x start_y
    max_width max_height =
  let max_w = max_width - child_size.width in
  let max_h = max_height - child_size.height in
  let half_x = max_w / 2 in
  let half_y = max_h / 2 in
  let start_x =
    start_x
    +
    if x_shift = 0.0 then half_x
    else if x_shift > 0.0 then
      half_x + int_of_float (float_of_int half_x *. x_shift)
    else
      let x_shift = x_shift *. -1.0 in
      let percent = float_of_int half_x *. x_shift in
      half_x - int_of_float percent
  in
  let start_y =
    start_y
    +
    if y_shift = 0.0 then half_y
    else if y_shift > 0.0 then
      half_y + int_of_float (float_of_int half_y *. y_shift)
    else
      let y_shift = y_shift *. -1.0 in
      let percent = float_of_int half_y *. y_shift in
      half_y - int_of_float percent
  in
  { start_x; start_y; max_w; max_h }

let draw x_shift y_shift child constraints =
  let child_size = Drawable.size constraints child in
  let properties =
    get_properties x_shift y_shift child_size constraints.start_x
      constraints.start_y constraints.max_width constraints.max_height
  in
  let child_constraints =
    {
      constraints with
      start_y = properties.start_y;
      start_x = properties.start_x;
    }
  in
  let _ = Drawable.draw child_constraints child in
  { width = properties.max_w; height = properties.max_h }

let update x_shift y_shift child constraints =
  let child_constraints = Constraints.drop_model constraints in
  let child_size = Drawable.size child_constraints child in
  let properties =
    get_properties x_shift y_shift child_size constraints.start_x
      constraints.start_y constraints.max_width constraints.max_height
  in
  let child_constraints =
    {
      constraints with
      start_y = properties.start_y;
      start_x = properties.start_x;
    }
  in
  let { model; _ } = Drawable.update_model child_constraints child in
  { width = properties.max_w; height = properties.max_h; model }

let widget ?(x_shift = 0.0) ?(y_shift = 0.0) child =
  let x_shift =
    if x_shift > 1.0 then 1.0 else if x_shift < -1.0 then -1.0 else x_shift
  in
  let y_shift =
    if y_shift > 1.0 then 1.0 else if x_shift < -1.0 then -1.0 else y_shift
  in
  Widget (draw x_shift y_shift child, size, update x_shift y_shift child)
