open Drawable

let size constraints =
  { width = constraints.max_width; height = constraints.max_height }

let draw x_shift y_shift child constraints =
  let child_size = Drawable.size constraints child in
  let max_w = constraints.max_width - child_size.width in
  let max_h = constraints.max_height - child_size.height in
  let half_x = max_w / 2 in
  let half_y = max_h / 2 in
  let start_x =
    constraints.start_x
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
    constraints.start_y
    +
    if y_shift = 0.0 then half_y
    else if y_shift > 0.0 then
      half_y + int_of_float (float_of_int half_y *. y_shift)
    else
      let y_shift = y_shift *. -1.0 in
      let percent = float_of_int half_y *. y_shift in
      half_y - int_of_float percent
  in
  let child_constraints = { constraints with start_x; start_y } in
  let _ = Drawable.draw child_constraints child in
  { width = max_w; height = max_h }

let widget ?(x_shift = 0.0) ?(y_shift = 0.0) child =
  let x_shift =
    if x_shift > 1.0 then 1.0 else if x_shift < -1.0 then -1.0 else x_shift
  in
  let y_shift =
    if y_shift > 1.0 then 1.0 else if x_shift < -1.0 then -1.0 else y_shift
  in
  Widget (draw x_shift y_shift child, size)
