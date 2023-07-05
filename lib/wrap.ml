open Drawable
open Column_row

type wrap_type = Min | SpaceAround | SpaceBetween

let get_direction_size caller constraints =
  match caller with
  | Row -> constraints.max_width
  | Column -> constraints.max_height

let get_child_size caller child_size =
  match caller with Row -> child_size.width | Column -> child_size.height

let wrap_inner caller wrap_type children =
  match caller with
  | Row -> (
      match wrap_type with
      | Min -> Row.min children
      | SpaceAround -> Row.space_around children
      | SpaceBetween -> Row.space_between children)
  | Column -> (
      match wrap_type with
      | Min -> Column.min children
      | SpaceAround -> Column.space_around children
      | SpaceBetween -> Column.space_between children)

let wrap_outer caller children =
  match caller with Row -> Column.min children | Column -> Row.min children

let split_children caller wrap_type children constraints =
  let direction_size = get_direction_size caller constraints in
  let rec get_line pos acc_els acc_size =
    if pos = Array.length children then
      let line =
        wrap_inner caller wrap_type (List.rev acc_els |> Array.of_list)
      in
      (pos - 1, line)
    else
      let child = Array.unsafe_get children pos in
      let child_size = size constraints child |> get_child_size caller in
      if child_size + acc_size <= direction_size then
        get_line (pos + 1) (child :: acc_els) (acc_size + child_size)
      else
        let line =
          wrap_inner caller wrap_type (List.rev acc_els |> Array.of_list)
        in
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

let size_row wrap_type children constraints =
  let children = split_children Row wrap_type children constraints in
  size constraints children

let draw_row wrap_type children constraints =
  let children = split_children Row wrap_type children constraints in
  draw constraints children

let row_min children = Widget (draw_row Min children, size_row Min children)

let row_space_around children =
  Widget (draw_row SpaceAround children, size_row SpaceAround children)

let row_space_between children =
  Widget (draw_row SpaceBetween children, size_row SpaceBetween children)

let size_column wrap_type children constraints =
  let children = split_children Column wrap_type children constraints in
  size constraints children

let draw_column wrap_type children constraints =
  let children = split_children Column wrap_type children constraints in
  draw constraints children

let column_min children =
  Widget (draw_column Min children, size_column Min children)

let column_space_around children =
  Widget (draw_column SpaceAround children, size_column SpaceAround children)

let column_space_between children =
  Widget (draw_column SpaceBetween children, size_column SpaceBetween children)
