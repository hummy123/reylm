(*
    This private module contains abstract functions for the Column and Row modules which have nearly identical logic.
*)

open Drawable

(* This type tells us which axis to draw along. *)
type caller = Row | Column

(* These functions below are for drawing with a flex child. *)
(* Helper functions. *)
let calc_remaining_space constraints flex_data = function
  | Column ->
      constraints.max_height - flex_data.occupied_non_flex_height
      |> float_of_int
  | Row ->
      constraints.max_width - flex_data.occupied_non_flex_width |> float_of_int

let calc_total_flex flex_data = function
  | Column -> flex_data.total_flex_height
  | Row -> flex_data.total_flex_width

let calc_start_pos constraints = function
  | Column -> constraints.start_y
  | Row -> constraints.start_x

let set_flex_child_constraints constraints el_size start_pos = function
  | Column -> { constraints with max_height = el_size; start_y = start_pos }
  | Row -> { constraints with max_width = el_size; start_x = start_pos }

let set_non_flex_constraints constraints start_pos = function
  | Column -> { constraints with start_y = start_pos }
  | Row -> { constraints with start_x = start_pos }

let increment_start_pos start_pos size = function
  | Column -> start_pos + size.height
  | Row -> start_pos + size.width

let get_start_pos constraints = function
  | Column -> constraints.start_y
  | Row -> constraints.start_x

let is_in_flex_direction flex_data = function
  | Column -> flex_data.num_flex_height_children > 0
  | Row -> flex_data.num_flex_width_children > 0

(* Main flex drawing function. *)
let flex_draw caller flex_data children constraints =
  let remaining_space = calc_remaining_space constraints flex_data caller in
  let total_flex = calc_total_flex flex_data caller in
  let start_pos = calc_start_pos constraints caller in
  let _ =
    Array.fold_left
      (fun start_pos el ->
        let constraints =
          match el with
          | Flex (flex_value, (Expand | FillHeight | FillWidth), _) ->
              let flex_percent = flex_value /. total_flex in
              let el_size =
                remaining_space *. flex_percent |> Float.round |> int_of_float
              in
              set_flex_child_constraints constraints el_size start_pos caller
          | _ -> set_non_flex_constraints constraints start_pos caller
        in
        let size = Drawable.draw constraints el in
        let start_pos = increment_start_pos start_pos size caller in
        start_pos)
      start_pos children
  in
  { height = constraints.max_height; width = constraints.max_width }

(*
    This function collapses constraints on the opposite axis.
    For a column, this means if constraints are collapsed, then the maximum width
    (including for FillWidth flex children) is the width of the largest non-flex child.

    For a row, this means if constraints are collapsed, then the maximum height is the height
    of the largest non-flex child.

    If the constraints aren't collapsed, then the return constraints are left alone.
*)
let collapse_constraints caller should_collapse flex_data constraints =
  if should_collapse then
    match caller with
    | Column -> { constraints with max_width = flex_data.max_child_width }
    | Row -> { constraints with max_height = flex_data.max_child_height }
  else constraints

(* Functions for drawing column or row at minimum size required by children,
   leaving empty space, unlike normal behaviour. *)
let min_size_internal flex_data constraints =
  let width =
    if flex_data.num_flex_width_children > 0 then constraints.max_width
    else flex_data.occupied_non_flex_width
  in
  let height =
    if flex_data.num_flex_height_children > 0 then constraints.max_height
    else flex_data.occupied_non_flex_height
  in
  { width; height }

let min_size should_collapse children caller constraints =
  let flex_data = Flex.calc_flex_data children constraints in
  let constraints =
    collapse_constraints caller should_collapse flex_data constraints
  in
  min_size_internal flex_data constraints

let min_draw should_collapse children caller constraints =
  let flex_data = Flex.calc_flex_data children constraints in
  let constraints =
    collapse_constraints caller should_collapse flex_data constraints
  in
  if is_in_flex_direction flex_data caller then
    flex_draw caller flex_data children constraints
  else
    let _ =
      Array.fold_left
        (fun start_pos el ->
          let constraints =
            set_non_flex_constraints constraints start_pos caller
          in
          let size = Drawable.draw constraints el in
          increment_start_pos start_pos size caller)
        (get_start_pos constraints caller)
        children
    in
    min_size_internal flex_data constraints

(* Functions for drawing column/row where height (if column) or width (if row) takes full constraints. *)
let max_size should_collapse children caller constraints =
  let { width; height } =
    min_size should_collapse children caller constraints
  in
  match caller with
  | Column -> { height = constraints.max_height; width }
  | Row -> { width = constraints.min_width; height }
