open Drawable_types
open Drawable_sizes

let row_center_or_end f_draw input lst calc_start_x =
  let occupied_width, max_height =
    List.fold_left
      (fun (acc_size, max_h) el ->
        let w, h = size input.parent_w input.parent_h el in
        let max_h = if h > max_h then h else max_h in
        (acc_size + w, max_h))
      (0, 0) lst
  in
  let start_x = input.parent_x + calc_start_x input.parent_w occupied_width in
  let _, _, state_tree, model =
    List.fold_left
      (fun (x_pos, acc_w, state_tree, model) el ->
        let max_w = input.parent_w - acc_w in
        let input =
          {
            parent_x = x_pos;
            parent_y = input.parent_y;
            parent_w = max_w;
            parent_h = input.parent_h;
            state_tree;
            model;
          }
        in
        let output = f_draw input el in
        ( x_pos + output.width,
          acc_w + output.width,
          output.state_tree,
          output.model ))
      (start_x, 0, input.state_tree, input.model)
      lst
  in
  { width = input.parent_w; height = max_height; state_tree; model }

let draw_start f_draw input lst =
  let _, _, h, state_tree, model =
    List.fold_left
      (fun (x_pos, acc_w, max_child_h, state_tree, model) el ->
        let max_w = input.parent_w - acc_w in
        let input =
          {
            parent_x = x_pos;
            parent_y = input.parent_y;
            parent_w = max_w;
            parent_h = input.parent_h;
            state_tree;
            model;
          }
        in
        let output = f_draw input el in
        let max_child_h =
          if output.height > max_child_h then output.height else max_child_h
        in
        ( x_pos + output.width,
          acc_w + output.width,
          max_child_h,
          output.state_tree,
          output.model ))
      (input.parent_x, 0, 0, input.state_tree, input.model)
      lst
  in
  { width = input.parent_w; height = h; state_tree; model }

let draw_center f_draw input lst =
  row_center_or_end f_draw input lst (fun parent_w occupied_width ->
      (parent_w / 2) - (occupied_width / 2))

let draw_end f_draw input lst =
  row_center_or_end f_draw input lst (fun parent_w occupied_width ->
      parent_w - occupied_width)

let draw_space_around f_draw input lst =
  let occupied_width, num_els, max_height =
    List.fold_left
      (fun (acc_size, num_els, max_height) el ->
        let w, h = size input.parent_w input.parent_h el in
        (acc_size + w, num_els + 1, if h > max_height then h else max_height))
      (0, 0, 0) lst
  in
  let unoccupied_width = input.parent_w - occupied_width in
  let hor_gap_size = unoccupied_width / num_els in
  let _, _, state_tree, model =
    List.fold_left
      (fun (x_pos, acc_w, state_tree, model) el ->
        let input =
          {
            parent_x = x_pos;
            parent_y = input.parent_y;
            parent_w = input.parent_w - acc_w;
            parent_h = input.parent_h;
            state_tree;
            model;
          }
        in
        let output = f_draw input el in
        let x_pos, acc_w =
          ( x_pos + output.width + hor_gap_size,
            acc_w + output.width + hor_gap_size )
        in
        (x_pos, acc_w, output.state_tree, output.model))
      (input.parent_x + (hor_gap_size / 2), 0, input.state_tree, input.model)
      lst
  in
  { width = input.parent_w; height = max_height; state_tree; model }

let draw_space_between f_draw input lst =
  let occupied_width, num_els, max_height =
    List.fold_left
      (fun (acc_size, num_els, max_height) el ->
        let w, h = size input.parent_w input.parent_h el in
        (acc_size + w, num_els + 1, if h > max_height then h else max_height))
      (0, 0, 0) lst
  in
  let unoccupied_width = input.parent_w - occupied_width in
  let hor_gap_size =
    if num_els <= 1 then 0 else unoccupied_width / (num_els - 1)
  in
  let _, _, state_tree, model =
    List.fold_left
      (fun (x_pos, acc_w, state_tree, model) el ->
        let input =
          {
            parent_x = x_pos;
            parent_y = input.parent_y;
            parent_w = input.parent_w - acc_w;
            parent_h = max_height;
            state_tree;
            model;
          }
        in
        let output = f_draw input el in
        let x_pos, acc_w =
          ( x_pos + output.width + hor_gap_size,
            acc_w + output.width + hor_gap_size )
        in
        (x_pos, acc_w, output.state_tree, output.model))
      (input.parent_y, 0, input.state_tree, input.model)
      lst
  in
  { width = input.parent_w; height = max_height; state_tree; model }
