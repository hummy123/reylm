open Drawable
open Flex

let flex_draw flex_data width children constraints =
  let remaining_space =
    float_of_int (constraints.max_height - flex_data.occupied_non_flex_height)
  in
  let total_flex = flex_data.total_flex_height in
  let _ =
    Array.fold_left
      (fun start_y el ->
        let constraints =
          match el with
          | Flex (flex_value, (FillHeight | Expand), _) ->
              let flex_percent = flex_value /. total_flex in
              let height = remaining_space *. flex_percent |> int_of_float in
              { constraints with max_height = height; start_y }
          | _ -> { constraints with start_y }
        in
        let size = Drawable.draw constraints el in
        let start_y = start_y + size.height in
        start_y)
      constraints.start_y children
  in
  { height = constraints.max_height; width }

(* Functions for drawing row at minimum size required by children,
   not expanding as with normal behaviour. *)
let min_size collapse_width children constraints =
  let height, width, has_flex =
    Array.fold_left
      (fun (acc_h, max_w, has_flex) el ->
        let child_is_flex =
          match el with
          | Flex (_, (Expand | FillHeight), _) -> true
          | _ -> false
        in
        let size = Drawable.size constraints el in
        let has_flex = child_is_flex || has_flex in
        let acc_h = acc_h + size.height in
        let max_w = max max_w size.width in
        (acc_h, max_w, has_flex))
      (0, 0, false) children
  in
  let width = if collapse_width then width else constraints.max_width in
  if has_flex then { width; height = constraints.max_height }
  else { width; height }

let min_draw collapse_width children constraints =
  let flex_data = calc_flex_data children constraints in
  let constraints =
    if collapse_width then
      { constraints with max_width = flex_data.max_child_width }
    else constraints
  in
  if flex_data.num_flex_height_children > 0 then
    flex_draw flex_data constraints.max_width children constraints
  else
    let _, height, width =
      Array.fold_left
        (fun (start_y, acc_h, max_w) el ->
          let constraints = { constraints with start_y } in
          let size = Drawable.draw constraints el in
          (* Values for next iteration. *)
          let start_y = start_y + size.height in
          let acc_h = size.height + acc_h in
          let max_h = max max_w size.width in
          (start_y, acc_h, max_h))
        (constraints.start_y, 0, 0)
        children
    in
    let width = if collapse_width then width else constraints.min_width in
    { width; height }

let min ?(collapse_width = false) children =
  Widget (min_draw collapse_width children, min_size collapse_width children)

(* Functions for drawing row at maximum size according to different directions. *)
let max_size collapse_height children constraints =
  let { height; _ } = min_size collapse_height children constraints in
  { width = constraints.max_width; height }

(* This abstracts some logicin each draw function below.
   It checks if any children are flex and, if so, they are provided to the flex_draw instead.*)
let flex_draw_if_flex_children collapse_height children constraints f_not_flex =
  let flex_data = calc_flex_data children constraints in
  let constraints =
    if collapse_height then
      { constraints with max_height = flex_data.max_child_height }
    else constraints
  in
  if flex_data.num_flex_width_children > 0 then
    flex_draw flex_data constraints.max_height children constraints
  else f_not_flex flex_data constraints

(*
    Generic function for aligning to a direction (left, center, right).
    calc_start_x function is the only thing changing for this, specifying x coordinate to start drawing from.
*)
let directional_draw calc_start_x collapse_height children constraints =
  let if_not_flex flex_data constraints =
    let start_x = constraints.start_x + calc_start_x constraints flex_data in
    let _ =
      Array.fold_left
        (fun start_x el ->
          let constraints = { constraints with start_x } in
          let size = Drawable.draw constraints el in
          (* Values for next iteration. *)
          let start_x = start_x + size.width in
          start_x)
        start_x children
    in
    { width = constraints.max_width; height = constraints.max_height }
  in
  flex_draw_if_flex_children collapse_height children constraints if_not_flex

(* Fuctions for drawing row aligned to left. *)
let calc_start_x_left _ _ = 0

let left ?(collapse_height = false) children =
  Widget
    ( directional_draw calc_start_x_left collapse_height children,
      max_size collapse_height children )

(* Functions for drawing row aligned to center. *)
let calc_start_x_center constraints flex_data =
  (constraints.max_width / 2) - (flex_data.occupied_non_flex_width / 2)

let center ?(collapse_height = false) children =
  Widget
    ( directional_draw calc_start_x_center collapse_height children,
      max_size collapse_height children )

(* Functions for drawing row aligned to right. *)
let calc_start_x_right constraints flex_data =
  constraints.max_width - flex_data.occupied_non_flex_width

let right ?(collapse_height = false) children =
  Widget
    ( directional_draw calc_start_x_right collapse_height children,
      max_size collapse_height children )

(* Functions for drawing row with space in between/around. *)
let spacer = Spacer.horizontal ()
let double_spacer = Spacer.horizontal ~flex_val:2. ()

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
    flex_draw flex_data constraints.max_height children constraints
  in
  flex_draw_if_flex_children collapse_height children constraints if_not_flex

let space_between ?(collapse_height = false) children =
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
    flex_draw flex_data constraints.max_height children constraints
  in
  flex_draw_if_flex_children collapse_height children constraints if_not_flex

let space_around ?(collapse_height = false) children =
  Widget
    ( draw_space_around collapse_height children,
      max_size collapse_height children )
