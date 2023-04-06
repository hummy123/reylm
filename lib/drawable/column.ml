open Drawable_types
open Drawable_sizes

let column_center_or_end f_draw input lst calc_start_y =
  let occupied_size, max_width =
    List.fold_left
      (fun (acc_size, max_w) el ->
        let w, h = size input.parent_w input.parent_h el in
        let max_w = if w > max_w then w else max_w in
        (acc_size + h, max_w))
      (0, 0) lst
  in
  let start_y = input.parent_y + calc_start_y input.parent_h occupied_size in
  let _, _, state_tree, model =
    List.fold_left
      (fun (y_pos, acc_h, state_tree, model) el ->
        let max_h = y_pos - acc_h in
        let input =
          {
            parent_x = input.parent_x;
            parent_y = y_pos;
            parent_w = max_width;
            parent_h = max_h;
            state_tree;
            model;
          }
        in
        let output = f_draw input el in
        ( y_pos + output.height,
          acc_h + output.height,
          output.state_tree,
          output.model ))
      (start_y, 0, input.state_tree, input.model)
      lst
  in
  { width = max_width; height = input.parent_h; state_tree; model }

let draw_start f_draw input lst =
  let _, width, _, state_tree, model =
    List.fold_left
      (fun (y_pos, max_child_w, acc_h, state_tree, model) el ->
        let max_h = input.parent_h - acc_h in
        let input =
          {
            parent_x = input.parent_x;
            parent_y = y_pos;
            parent_w = input.parent_w;
            parent_h = max_h;
            model;
            state_tree;
          }
        in
        let output = f_draw input el in
        let max_child_w =
          if output.width > max_child_w then output.width else max_child_w
        in
        ( y_pos + output.height,
          max_child_w,
          acc_h + output.height,
          output.state_tree,
          output.model ))
      (input.parent_y, 0, 0, input.state_tree, input.model)
      lst
  in
  { width; height = input.parent_h; state_tree; model }

let draw_center f_draw input lst =
  column_center_or_end f_draw input lst (fun parent_h occupied_size ->
      (parent_h / 2) - (occupied_size / 2))

let draw_end f_draw input lst =
  column_center_or_end f_draw input lst (fun parent_h occupied_size ->
      parent_h - occupied_size)

let draw_space_around f_draw input lst =
  let occupied_height, num_els, max_width =
    List.fold_left
      (fun (acc_size, num_els, max_width) el ->
        let w, h = size input.parent_w input.parent_h el in
        (acc_size + h, num_els + 1, if w > max_width then w else max_width))
      (0, 0, 0) lst
  in
  let unoccupied_size = input.parent_h - occupied_height in
  let vert_gap_size = unoccupied_size / num_els in
  let _, _, state_tree, model =
    List.fold_left
      (fun (y_pos, acc_h, state_tree, model) el ->
        let input =
          {
            parent_x = input.parent_x;
            parent_y = y_pos;
            parent_w = max_width;
            parent_h = input.parent_h - acc_h;
            state_tree;
            model;
          }
        in
        let output = f_draw input el in
        let y_pos, acc_h =
          ( y_pos + output.height + vert_gap_size,
            acc_h + output.height + vert_gap_size )
        in
        (y_pos, acc_h, output.state_tree, output.model))
      (input.parent_y + (vert_gap_size / 2), 0, input.state_tree, input.model)
      lst
  in
  { width = max_width; height = input.parent_h; state_tree; model }

let draw_space_between f_draw input lst =
  let occupied_height, num_els, max_width =
    List.fold_left
      (fun (acc_size, num_els, max_width) el ->
        let w, h = size input.parent_w input.parent_h el in
        (acc_size + h, num_els + 1, if w > max_width then w else max_width))
      (0, 0, 0) lst
  in
  let unoccupied_size = input.parent_h - occupied_height in
  let vert_gap_size =
    if num_els <= 1 then 0 else unoccupied_size / (num_els - 1)
  in
  let _, _, state_tree, model =
    List.fold_left
      (fun (y_pos, acc_h, state_tree, model) el ->
        let input =
          {
            parent_x = input.parent_x;
            parent_y = y_pos;
            parent_w = max_width;
            parent_h = input.parent_x - acc_h;
            state_tree;
            model;
          }
        in
        let output = f_draw input el in
        let y_pos, acc_h =
          ( y_pos + output.height + vert_gap_size,
            acc_h + output.height + vert_gap_size )
        in
        (y_pos, acc_h, output.state_tree, output.model))
      (input.parent_y, 0, input.state_tree, input.model)
      lst
  in
  { width = max_width; height = input.parent_h; state_tree; model }
