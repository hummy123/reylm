open Drawable

(* Functions for drawing row at minimum size required by children,
   not expanding as with normal behaviour. *)
let min_size children constraints =
  let width, height =
    Array.fold_left
      (fun (acc_w, max_h) el ->
        let size = Drawable.size constraints el in
        (acc_w + size.width, max max_h size.height))
      (0, 0) children
  in
  { width; height }

let min_draw children constraints =
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

(* Functions for drawing row at maximum size. *)
let max_size children constraints =
  let { height; _ } = min_size children constraints in
  { width = constraints.max_width; height }

let calc_flex_data children constraints =
  Array.fold_left
    (fun flex_data el ->
      match el with
      | Flex (flex_value, _, _) ->
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

let start_draw children constraints =
  let flex_data = calc_flex_data children constraints in
  let remaining_space =
    float_of_int
      (constraints.max_width - flex_data.occupied_space_without_flex_children)
  in
  let total_flex = float_of_int flex_data.total_flex in
  let _, width, height =
    Array.fold_left
      (fun (start_x, acc_w, max_h) el ->
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
        let acc_w = size.width + acc_w in
        let max_h = max max_h size.height in
        (start_x, acc_w, max_h))
      (constraints.start_x, 0, 0)
      children
  in
  { width; height }

let start children = Widget (start_draw children, max_size children)
