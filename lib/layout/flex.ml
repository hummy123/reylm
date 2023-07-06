open Constraints
open Drawable

let expand ?(flex_val = 1.) child = Flex (flex_val, Expand, child)
let fill_height ?(flex_val = 1.) child = Flex (flex_val, FillHeight, child)
let fill_width ?(flex_val = 1.) child = Flex (flex_val, FillWidth, child)
let natural_size ?(flex_val = 1.) child = Flex (flex_val, NaturalSize, child)

let calc_flex_data children (constraints : Constraints.input_constraints) =
  Array.fold_left
    (fun flex_data el ->
      match el with
      | Flex (flex_value, Expand, _) ->
          let total_flex_height = flex_data.total_flex_height +. flex_value in
          let total_flex_width = flex_data.total_flex_width +. flex_value in
          let num_flex_width_children = flex_data.num_flex_width_children + 1 in
          let num_flex_height_children =
            flex_data.num_flex_height_children + 1
          in
          {
            flex_data with
            total_flex_width;
            total_flex_height;
            num_flex_height_children;
            num_flex_width_children;
          }
      | Flex (flex_value, FillWidth, _) ->
          let total_flex_width = flex_data.total_flex_width +. flex_value in
          let num_flex_width_children = flex_data.num_flex_width_children + 1 in
          let ({ height; _ } : drawable_size) = Drawable.size constraints el in
          let max_child_height = max height flex_data.max_child_height in
          let occupied_non_flex_height =
            flex_data.occupied_non_flex_height + height
          in
          {
            flex_data with
            total_flex_width;
            num_flex_width_children;
            max_child_height;
            occupied_non_flex_height;
          }
      | Flex (flex_value, FillHeight, _) ->
          let total_flex_height = flex_data.total_flex_height +. flex_value in
          let num_flex_height_children =
            flex_data.num_flex_height_children + 1
          in
          let ({ width; _ } : drawable_size) = Drawable.size constraints el in
          let max_child_width = max width flex_data.max_child_width in
          let occupied_non_flex_width =
            flex_data.occupied_non_flex_width + width
          in
          {
            flex_data with
            total_flex_height;
            num_flex_height_children;
            max_child_width;
            occupied_non_flex_width;
          }
      | _ ->
          let ({ width; height } : drawable_size) =
            Drawable.size constraints el
          in
          let occupied_non_flex_width =
            flex_data.occupied_non_flex_width + width
          in
          let occupied_non_flex_height =
            flex_data.occupied_non_flex_height + height
          in
          let max_child_width = max width flex_data.max_child_width in
          let max_child_height = max height flex_data.max_child_height in
          {
            flex_data with
            occupied_non_flex_width;
            occupied_non_flex_height;
            max_child_width;
            max_child_height;
          })
    initial_flex_data children
