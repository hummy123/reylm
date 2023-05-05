open Drawable
open Flex
open Column_row

(* Functions for drawing row at minimum size required by children,
   not expanding as with normal behaviour. *)
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

let min_size collapse_width children constraints =
  let flex_data = calc_flex_data children constraints in
  let constraints =
    Column_row.collapse_constraints Column collapse_width flex_data constraints
  in
  min_size_internal flex_data constraints

let min_draw collapse_width children constraints =
  let flex_data = calc_flex_data children constraints in
  let constraints =
    Column_row.collapse_constraints Column collapse_width flex_data constraints
  in
  if flex_data.num_flex_height_children > 0 then
    flex_draw Column flex_data children constraints
  else
    let _ =
      Array.fold_left
        (fun start_y el ->
          let constraints = { constraints with start_y } in
          let size = Drawable.draw constraints el in
          start_y + size.height)
        constraints.start_y children
    in
    min_size_internal flex_data constraints

let min ?(collapse_width = true) children =
  Widget (min_draw collapse_width children, min_size collapse_width children)

(* Functions for drawing row at maximum size according to different directions. *)
let max_size collapse_width children constraints =
  let { width; _ } = min_size collapse_width children constraints in
  { height = constraints.max_height; width }

(* This abstracts some logicin each draw function below.
   It checks if any children are flex and, if so, they are provided to the flex_draw instead.*)
let flex_draw_if_flex_children collapse_width children constraints f_not_flex =
  let flex_data = calc_flex_data children constraints in
  let constraints =
    if collapse_width then
      { constraints with max_width = flex_data.max_child_width }
    else constraints
  in
  if flex_data.num_flex_height_children > 0 then
    flex_draw Column flex_data children constraints
  else f_not_flex flex_data constraints

(*
    Generic function for aligning to a direction (left, center, right).
    calc_start_x function is the only thing changing for this, specifying x coordinate to start drawing from.
*)
let directional_draw calc_start_y collapse_width children constraints =
  let if_not_flex flex_data constraints =
    let start_y = constraints.start_y + calc_start_y constraints flex_data in
    let _ =
      Array.fold_left
        (fun start_y el ->
          let constraints = { constraints with start_y } in
          let size = Drawable.draw constraints el in
          start_y + size.height)
        start_y children
    in
    { width = constraints.max_width; height = constraints.max_height }
  in
  flex_draw_if_flex_children collapse_width children constraints if_not_flex

(* Fuctions for drawing row aligned to left. *)
let calc_start_y_left _ _ = 0

let top ?(collapse_width = true) children =
  Widget
    ( directional_draw calc_start_y_left collapse_width children,
      max_size collapse_width children )

(* Functions for drawing row aligned to center. *)
let calc_start_y_center constraints flex_data =
  (constraints.max_height / 2) - (flex_data.occupied_non_flex_height / 2)

let center ?(collapse_width = true) children =
  Widget
    ( directional_draw calc_start_y_center collapse_width children,
      max_size collapse_width children )

(* Functions for drawing row aligned to right. *)
let calc_start_y_right constraints flex_data =
  constraints.max_height - flex_data.occupied_non_flex_height

let bottom ?(collapse_width = true) children =
  Widget
    ( directional_draw calc_start_y_right collapse_width children,
      max_size collapse_width children )

(* Functions for drawing row with space in between/around. *)
let spacer = Spacer.vertical ()
let double_spacer = Spacer.vertical ~flex_val:2. ()

let draw_space_between collapse_height children constraints =
  let if_not_flex _ constraints =
    (* We will insert a spacer in between each child. *)
    let children =
      Array.fold_right (fun el acc -> spacer :: el :: acc) children []
    in
    (* Remove first spacer from list. *)
    let children =
      match children with _ :: tail -> tail |> Array.of_list | _ -> [||]
    in
    let flex_data = calc_flex_data children constraints in
    flex_draw Column flex_data children constraints
  in
  flex_draw_if_flex_children collapse_height children constraints if_not_flex

let space_between ?(collapse_height = true) children =
  Widget
    ( draw_space_between collapse_height children,
      max_size collapse_height children )

let draw_space_around collapse_height children constraints =
  let if_not_flex _ constraints =
    let children =
      Array.fold_right
        (fun el acc -> double_spacer :: el :: acc)
        children [ spacer ]
      |> Array.of_list
    in
    Array.unsafe_set children 0 spacer;
    let flex_data = calc_flex_data children constraints in
    flex_draw Column flex_data children constraints
  in
  flex_draw_if_flex_children collapse_height children constraints if_not_flex

let space_around ?(collapse_height = true) children =
  Widget
    ( draw_space_around collapse_height children,
      max_size collapse_height children )
