open Drawable

(*
    Flex functions implemented at the start so any other functions that find a child in a flex can call this one.
    This makes sense because flex, by definition, takes all available space, so there is only one possible way
    to lay out children.
    It simplifies code as well as other fuctions don't need to consider flex.
*)

let calc_flex_data children constraints =
  Array.fold_left
    (fun flex_data el ->
      match el with
      | Flex (flex_value, FillWidth, _) ->
          let total_flex = flex_data.total_flex + flex_value in
          let num_flex_children = flex_data.num_flex_children + 1 in
          { flex_data with total_flex; num_flex_children }
      | _ ->
          let { width; _ } = Drawable.size constraints el in
          let occupied_space_without_flex_children =
            flex_data.occupied_space_without_flex_children + width
          in
          { flex_data with occupied_space_without_flex_children })
    initial_flex_data children

let flex_draw flex_data children constraints =
  let remaining_space =
    float_of_int
      (constraints.max_width - flex_data.occupied_space_without_flex_children)
  in
  let total_flex = float_of_int flex_data.total_flex in
  let _, height =
    Array.fold_left
      (fun (start_x, max_h) el ->
        let constraints =
          match el with
          | Flex (flex_value, _, _) ->
              let flex_value = float_of_int flex_value in
              let flex_percent = flex_value /. total_flex in
              let width = remaining_space *. flex_percent |> int_of_float in
              { constraints with max_width = width; start_x }
          | _ -> { constraints with start_x }
        in
        let size = Drawable.draw constraints el in
        let start_x = start_x + size.width in
        let max_h = max max_h size.height in
        (start_x, max_h))
      (constraints.start_x, 0) children
  in
  { width = constraints.max_width; height }

(* Functions for drawing row at minimum size required by children,
   not expanding as with normal behaviour. *)
let min_size children constraints =
  let width, height, has_flex =
    Array.fold_left
      (fun (acc_w, max_h, has_flex) el ->
        let child_is_flex = match el with Flex _ -> true | _ -> false in
        let size = Drawable.size constraints el in
        let has_flex = child_is_flex || has_flex in
        (acc_w + size.width, max max_h size.height, has_flex))
      (0, 0, false) children
  in
  if has_flex then { width = constraints.max_width; height }
  else { width; height }

let min_draw children constraints =
  let flex_data = calc_flex_data children constraints in
  if flex_data.num_flex_children > 0 then
    flex_draw flex_data children constraints
  else
    let _, width, height =
      Array.fold_left
        (fun (start_x, acc_w, max_h) el ->
          let constraints = { constraints with start_x } in
          let size = Drawable.draw constraints el in
          (* Values for next iteration. *)
          let start_x = start_x + size.width in
          let acc_w = size.width + acc_w in
          let max_h = max max_h size.height in
          (start_x, acc_w, max_h))
        (constraints.start_x, 0, 0)
        children
    in
    { width; height }

let min children = Widget (min_draw children, min_size children)

(* Functions for drawing row at maximum size according to different directions. *)
let max_size children constraints =
  let { height; _ } = min_size children constraints in
  { width = constraints.max_width; height }

let start_draw children constraints =
  let flex_data = calc_flex_data children constraints in
  if flex_data.num_flex_children > 0 then
    flex_draw flex_data children constraints
  else
    let _, height =
      Array.fold_left
        (fun (start_x, max_h) el ->
          let constraints = { constraints with start_x } in
          let size = Drawable.draw constraints el in
          (* Values for next iteration. *)
          let start_x = start_x + size.width in
          let max_h = max max_h size.height in
          (start_x, max_h))
        (constraints.start_x, 0) children
    in
    { width = constraints.max_width; height }