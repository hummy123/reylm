open Drawable

let expand ?(flex_val = 1) child = Flex (flex_val, Expand, child)
let fill_height ?(flex_val = 1) child = Flex (flex_val, FillHeight, child)
let fill_width ?(flex_val = 1) child = Flex (flex_val, FillWidth, child)
let natural_size ?(flex_val = 1) child = Flex (flex_val, NaturalSize, child)

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
