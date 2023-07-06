open Constraints
open Drawable

let get_child_constraints l t r b constraints =
  let max_width = constraints.max_width - l - r in
  let min_width = constraints.min_width - l - r in
  let max_height = constraints.max_height - t - b in
  let min_height = constraints.min_height - t - r in
  {
    start_x = constraints.start_x + l;
    start_y = constraints.start_y + t;
    max_width;
    min_width;
    max_height;
    min_height;
  }

let size l t r b child constraints =
  let child_constraints = get_child_constraints l t r b constraints in
  let ({ width; height } : drawable_size) =
    Drawable.size child_constraints child
  in
  { width = width + l + r; height = height + t + b }

let draw l t r b child constraints =
  let child_constraints = get_child_constraints l t r b constraints in
  let ({ width; height } : drawable_size) =
    Drawable.draw child_constraints child
  in
  { width = width + l + r; height = height + t + b }

let update l t r b child constraints model =
  let child_constraints = get_child_constraints l t r b constraints in
  let { width; height; model } =
    Drawable.update child_constraints model child
  in
  { model; width = width + l + r; height = height + t + b }

let from_ltrb ?(left = 0) ?(top = 0) ?(right = 0) ?(bottom = 0) child =
  Widget
    ( draw left top right bottom child,
      size left top right bottom child,
      update left top right bottom child )

let by_axis ?(vertical = 0) ?(horizontal = 0) child =
  Widget
    ( draw horizontal vertical horizontal vertical child,
      size horizontal vertical horizontal vertical child,
      update horizontal vertical horizontal vertical child )

let all pad_by child =
  Widget
    ( draw pad_by pad_by pad_by pad_by child,
      size pad_by pad_by pad_by pad_by child,
      update pad_by pad_by pad_by pad_by child )
