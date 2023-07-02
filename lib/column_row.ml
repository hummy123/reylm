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

let set_start_pos constraints start_pos = function
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
          | _ -> set_start_pos constraints start_pos caller
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
let get_flex_width_height flex_data constraints =
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
  get_flex_width_height flex_data constraints

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
          let constraints = set_start_pos constraints start_pos caller in
          let size = Drawable.draw constraints el in
          increment_start_pos start_pos size caller)
        (get_start_pos constraints caller)
        children
    in
    get_flex_width_height flex_data constraints

(* Functions for drawing column/row where height (if column) or width (if row) takes full constraints. *)
let max_size should_collapse children caller constraints =
  let flex_data = Flex.calc_flex_data children constraints in
  let constraints =
    collapse_constraints caller should_collapse flex_data constraints
  in
  { height = constraints.max_height; width = constraints.max_width }

(* This abstracts some logicin each draw function below.
   It checks if any children are flex and, if so, they are provided to the flex_draw instead.*)
let flex_draw_if_flex_children should_collapse children caller constraints
    f_not_flex =
  let flex_data = Flex.calc_flex_data children constraints in
  let constraints =
    collapse_constraints caller should_collapse flex_data constraints
  in
  match caller with
  | Column ->
      if flex_data.num_flex_height_children > 0 then
        flex_draw caller flex_data children constraints
      else f_not_flex flex_data constraints
  | Row ->
      if flex_data.num_flex_width_children > 0 then
        flex_draw caller flex_data children constraints
      else f_not_flex flex_data constraints

(*
    Generic function for aligning to a direction (left, center, right).
    calc_start_x function is the only thing changing for this, specifying x coordinate to start drawing from.
*)
let directional_draw calc_start should_collapse children caller constraints =
  let if_not_flex flex_data constraints =
    let start_pos =
      get_start_pos constraints caller + calc_start constraints flex_data
    in
    let _ =
      Array.fold_left
        (fun start_pos el ->
          let constraints = set_start_pos constraints start_pos caller in
          let size = Drawable.draw constraints el in
          increment_start_pos start_pos size caller)
        start_pos children
    in
    { width = constraints.max_width; height = constraints.max_height }
  in
  flex_draw_if_flex_children should_collapse children caller constraints
    if_not_flex

let calc_when_start _ _ = 0

let calc_when_center caller constraints flex_data =
  match caller with
  | Column ->
      (constraints.max_height / 2) - (flex_data.occupied_non_flex_height / 2)
  | Row -> (constraints.max_width / 2) - (flex_data.occupied_non_flex_width / 2)

let calc_when_end caller constraints flex_data =
  match caller with
  | Column -> constraints.max_height - flex_data.occupied_non_flex_height
  | Row -> constraints.max_width - flex_data.occupied_non_flex_width

(* Functions for drawing column/row with space between or space around. *)
let row_spacer = Spacer.horizontal ()
let col_spacer = Spacer.vertical ()
let get_spacer = function Column -> col_spacer | Row -> row_spacer

let draw_space_between should_collapse children caller constraints =
  let if_not_flex _ constraints =
    let spacer = get_spacer caller in
    (* We will insert a spacer in between each child. *)
    let children =
      Array.fold_right (fun el acc -> spacer :: el :: acc) children []
    in
    (* Remove first spacer from list. *)
    let children =
      match children with _ :: tail -> tail |> Array.of_list | _ -> [||]
    in
    let flex_data = Flex.calc_flex_data children constraints in
    flex_draw caller flex_data children constraints
  in
  flex_draw_if_flex_children should_collapse children caller constraints
    if_not_flex

let draw_space_around should_collapse children caller constraints =
  (* if_not_flex function has to add spacing by itself,
     because the same spacer trick used in space_between doesn't work for space_around. *)
  let if_not_flex flex_data constraints =
    let remaining_space = calc_remaining_space constraints flex_data caller in
    let start_pos = calc_start_pos constraints caller in
    let space = int_of_float remaining_space / Array.length children in
    let _ =
      Array.fold_left
        (fun start_pos el ->
          let constraints = set_start_pos constraints start_pos caller in
          let size = Drawable.draw constraints el in
          increment_start_pos (start_pos + space) size caller)
        (start_pos + (space / 2))
        children
    in
    { width = constraints.max_width; height = constraints.max_height }
  in
  flex_draw_if_flex_children should_collapse children caller constraints
    if_not_flex
