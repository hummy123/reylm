open Drawable
open Column_row

let get_direction_size caller constraints =
  match caller with
  | Row -> constraints.max_width
  | Column -> constraints.max_height

let get_child_size caller child_size =
  match caller with Row -> child_size.width | Column -> child_size.height

let wrap_inner caller children =
  match caller with Row -> Row.min children | Column -> Column.min children

let wrap_outer caller children =
  match caller with Row -> Column.min children | Column -> Row.min children

let split_children caller children constraints =
  let direction_size = get_direction_size caller constraints in
  let rec get_line pos acc_els acc_size =
    if pos = Array.length children then
      let line = wrap_inner caller (List.rev acc_els |> Array.of_list) in
      (pos - 1, line)
    else
      let child = Array.unsafe_get children pos in
      let child_size = size constraints child |> get_child_size caller in
      if child_size + acc_size <= direction_size then
        get_line (pos + 1) (child :: acc_els) (acc_size + child_size)
      else
        let line = wrap_inner caller (List.rev acc_els |> Array.of_list) in
        (pos - 1, line)
  in
  let rec collect_lines pos acc =
    if pos = Array.length children then
      wrap_outer caller (acc |> List.rev |> Array.of_list)
    else
      let pos, row = get_line pos [] 0 in
      collect_lines (pos + 1) (row :: acc)
  in
  collect_lines 0 []

let size_row children constraints =
  let children = split_children Row children constraints in
  size constraints children

let draw_row children constraints =
  let children = split_children Row children constraints in
  draw constraints children

let row children = Widget (draw_row children, size_row children)

let size_column children constraints =
  let children = split_children Column children constraints in
  size constraints children

let draw_column children constraints =
  let children = split_children Column children constraints in
  draw constraints children

let column children = Widget (draw_column children, size_column children)
