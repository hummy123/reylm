open Constraints
open Drawable

(* Row/column preprocessing data and function for calculating flex values. *)
type flex_data = {
  total_flex_height : float;
  total_flex_width : float;
  occupied_non_flex_height : int;
  occupied_non_flex_width : int;
  num_flex_width_children : int;
  num_flex_height_children : int;
  max_child_height : int;
  max_child_width : int;
  num_widthless_children : int;
  num_heightless_children : int;
}

let initial_flex_data =
  {
    total_flex_height = 0.;
    total_flex_width = 0.;
    num_flex_height_children = 0;
    num_flex_width_children = 0;
    occupied_non_flex_height = 0;
    occupied_non_flex_width = 0;
    max_child_height = 0;
    max_child_width = 0;
    num_widthless_children = 0;
    num_heightless_children = 0;
  }

let calc_flex_data children constraints =
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
            num_widthless_children =
              (if width > 0 then flex_data.num_widthless_children
               else flex_data.num_widthless_children + 1);
            num_heightless_children =
              (if height > 0 then flex_data.num_heightless_children
               else flex_data.num_heightless_children + 1);
          })
    initial_flex_data children
