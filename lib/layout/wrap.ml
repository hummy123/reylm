open Column_row
open Constraints
open Drawable

type wrap_type = Min | SpaceAround | SpaceBetween | Start | End

let get_direction_size caller constraints =
  match caller with
  | Row -> constraints.max_width
  | Column -> constraints.max_height

let get_child_size caller (child_size : drawable_size) =
  match caller with Row -> child_size.width | Column -> child_size.height

let wrap_inner caller wrap_type children =
  match caller with
  | Row -> (
      match wrap_type with
      | Min -> Row.min children
      | SpaceAround -> Row.space_around children
      | SpaceBetween ->
          (* Row.space_between children *)
          failwith ""
      | Start -> Row.left ~collapse_height:true children
      | End -> Row.right ~collapse_height:true children)
  | Column -> (
      match wrap_type with
      | Min -> Column.min children
      | SpaceAround -> Column.space_around children
      | SpaceBetween ->
          (* Column.space_between children *)
          failwith ""
      | Start -> Column.top ~collapse_width:true children
      | End -> Column.bottom ~collapse_width:true children)

let wrap_outer caller children =
  match caller with Row -> Column.min children | Column -> Row.min children

let split_children caller wrap_type line_padding children constraints =
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
      let pos, line = get_line pos [] 0 in
      collect_lines (pos + 1) (Padding.all line_padding line :: acc)
  in
  collect_lines 0 []

(* Row functions. *)
let size_row wrap_type row_padding children constraints =
  let children =
    split_children Row wrap_type row_padding children constraints
  in
  size constraints children

let draw_row wrap_type row_padding children constraints =
  let children =
    split_children Row wrap_type row_padding children constraints
  in
  draw constraints children

let update_row wrap_type row_padding children constraints model =
  let children =
    split_children Row wrap_type row_padding children constraints
  in
  update constraints model children

let row_min ?(row_padding = 0) children =
  Widget
    ( draw_row Min row_padding children,
      size_row Min row_padding children,
      update_row Min row_padding children )

let row_left ?(row_padding = 0) children =
  Widget
    ( draw_row Start row_padding children,
      size_row Start row_padding children,
      update_row Start row_padding children )

let row_right ?(row_padding = 0) children =
  Widget
    ( draw_row End row_padding children,
      size_row End row_padding children,
      update_row End row_padding children )

let row_space_around ?(row_padding = 0) children =
  Widget
    ( draw_row SpaceAround row_padding children,
      size_row SpaceAround row_padding children,
      update_row SpaceAround row_padding children )

(* let row_space_between ?(row_padding = 0) children = *)
(*   Widget *)
(*     ( draw_row SpaceBetween row_padding children, *)
(*       size_row SpaceBetween row_padding children ) *)

(* Column functions. *)
let size_column wrap_type col_padding children constraints =
  let children =
    split_children Column wrap_type col_padding children constraints
  in
  size constraints children

let draw_column wrap_type col_padding children constraints =
  let children =
    split_children Column wrap_type col_padding children constraints
  in
  draw constraints children

let update_column wrap_type col_padding children constraints model =
  let children =
    split_children Column wrap_type col_padding children constraints
  in
  update constraints model children

let column_min ?(col_padding = 0) children =
  Widget
    ( draw_column Min col_padding children,
      size_column Min col_padding children,
      update_column Min col_padding children )

let column_top ?(col_padding = 0) children =
  Widget
    ( draw_column Start col_padding children,
      size_column Start col_padding children,
      update_column Start col_padding children )

let column_bottom ?(col_padding = 0) children =
  Widget
    ( draw_column End col_padding children,
      size_column Min col_padding children,
      update_column End col_padding children )

let column_space_around ?(col_padding = 0) children =
  Widget
    ( draw_column SpaceAround col_padding children,
      size_column SpaceAround col_padding children,
      update_column SpaceAround col_padding children )

(* let column_space_between ?(col_padding = 0) children = *)
(*   Widget *)
(*     ( draw_column SpaceBetween col_padding children, *)
(*       size_column SpaceBetween col_padding children ) *)
