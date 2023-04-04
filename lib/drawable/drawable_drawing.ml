open Drawable_types
open Drawable_sizes

let rec draw_widget parent_x parent_y parent_w parent_h state_tree model =
  (* Abstract some common drawing logic in inner functions. *)
  let column_center_or_end lst calc_start_y =
    let occupied_size, max_width =
      List.fold_left
        (fun (acc_size, max_w) el ->
          let w, h = size parent_w parent_h el in
          let max_w = if w > max_w then w else max_w in
          (acc_size + h, max_w))
        (0, 0) lst
    in
    let start_y = parent_y + calc_start_y parent_h occupied_size in
    let _, _, state_tree, model =
      List.fold_left
        (fun (y_pos, acc_h, state_tree, model) el ->
          let max_h = y_pos - acc_h in
          let _, h, state_tree, model =
            draw_widget parent_x y_pos max_width max_h state_tree model el
          in
          (y_pos + h, acc_h + h, state_tree, model))
        (start_y, 0, state_tree, model)
        lst
    in
    (max_width, parent_h, state_tree, model)
  in
  let row_center_or_end lst calc_start_x =
    let occupied_width, max_height =
      List.fold_left
        (fun (acc_size, max_h) el ->
          let w, h = size parent_w parent_h el in
          let max_h = if h > max_h then h else max_h in
          (acc_size + w, max_h))
        (0, 0) lst
    in
    let start_x = parent_x + calc_start_x parent_w occupied_width in
    let _, _, state_tree, model =
      List.fold_left
        (fun (x_pos, acc_w, state_tree, model) el ->
          let max_w = parent_w - acc_w in
          let w, _, state_tree, model =
            draw_widget x_pos parent_y max_w parent_h state_tree model el
          in
          (x_pos + w, acc_w + w, state_tree, model))
        (start_x, 0, state_tree, model)
        lst
    in
    (parent_w, max_height, state_tree, model)
  in
  function
  | Empty -> (0, 0, state_tree, model)
  | HLine col ->
      Raylib.draw_line parent_x parent_y (parent_x + parent_w) parent_y col;
      (parent_w, 1, state_tree, model)
  | VLine col ->
      Raylib.draw_line parent_x parent_y parent_x (parent_y + parent_h) col;
      (1, parent_h, state_tree, model)
  | Border (r, c, t, d) ->
      let w, h, state_tree, model =
        draw_widget parent_x parent_y parent_w parent_h state_tree model d
      in
      let rect =
        Raylib.Rectangle.create (float_of_int parent_x) (float_of_int parent_y)
          (float_of_int w) (float_of_int h)
      in
      Raylib.draw_rectangle_rounded_lines rect r 10 t c;
      (w, h, state_tree, model)
  | Overlay d ->
      List.fold_left
        (fun (max_w, max_h, state_tree, model) el ->
          let w, h, state_tree, model =
            draw_widget parent_x parent_y parent_w parent_h state_tree model el
          in
          let w = if w > max_w then w else max_w in
          let h = if h > max_h then h else max_h in
          (w, h, state_tree, model))
        (0, 0, state_tree, model) d
  | Rect (w, h, r, c, d) ->
      let w = if w < parent_w then w else parent_w in
      let h = if h < parent_h then h else parent_h in
      let rect =
        Raylib.Rectangle.create (float_of_int parent_x) (float_of_int parent_y)
          (float_of_int w) (float_of_int h)
      in
      Raylib.draw_rectangle_rounded rect r 0 c;
      let _, _, state_tree, model =
        draw_widget parent_x parent_y w h state_tree model d
      in
      (w, h, state_tree, model)
  | ColumnStart lst ->
      let _, w, _, state_tree, model =
        List.fold_left
          (fun (y_pos, max_child_w, acc_h, state_tree, model) el ->
            let max_h = parent_h - acc_h in
            let w, h, state_tree, model =
              draw_widget parent_x y_pos parent_w max_h state_tree model el
            in
            let max_child_w = if w > max_child_w then w else max_child_w in
            (y_pos + h, max_child_w, acc_h + h, state_tree, model))
          (parent_y, 0, 0, state_tree, model)
          lst
      in
      (w, parent_h, state_tree, model)
  | ColumnCenter lst ->
      column_center_or_end lst (fun parent_h occupied_size ->
          (parent_h / 2) - (occupied_size / 2))
  | ColumnEnd lst ->
      column_center_or_end lst (fun parent_h occupied_size ->
          parent_h - occupied_size)
  | ColumnSpaceAround lst ->
      let occupied_height, num_els, max_width =
        List.fold_left
          (fun (acc_size, num_els, max_width) el ->
            let w, h = size parent_w parent_h el in
            (acc_size + h, num_els + 1, if w > max_width then w else max_width))
          (0, 0, 0) lst
      in
      let unoccupied_size = parent_h - occupied_height in
      let vert_gap_size = unoccupied_size / num_els in
      let _, _, state_tree, model =
        List.fold_left
          (fun (y_pos, acc_h, state_tree, model) el ->
            let _, h, state_tree, model =
              draw_widget parent_x y_pos max_width (parent_h - acc_h) state_tree
                model el
            in
            let y_pos, acc_h =
              (y_pos + h + vert_gap_size, acc_h + h + vert_gap_size)
            in
            (y_pos, acc_h, state_tree, model))
          (parent_y + (vert_gap_size / 2), 0, state_tree, model)
          lst
      in
      (max_width, parent_h, state_tree, model)
  | ColumnSpaceBetween lst ->
      let occupied_height, num_els, max_width =
        List.fold_left
          (fun (acc_size, num_els, max_width) el ->
            let w, h = size parent_w parent_h el in
            (acc_size + h, num_els + 1, if w > max_width then w else max_width))
          (0, 0, 0) lst
      in
      let unoccupied_size = parent_h - occupied_height in
      let vert_gap_size =
        if num_els <= 1 then 0 else unoccupied_size / (num_els - 1)
      in
      let _, _, state_tree, model =
        List.fold_left
          (fun (y_pos, acc_h, state_tree, model) el ->
            let _, h, state_tree, model =
              draw_widget parent_x y_pos max_width (parent_h - acc_h) state_tree
                model el
            in
            let y_pos, acc_h =
              (y_pos + h + vert_gap_size, acc_h + h + vert_gap_size)
            in
            (y_pos, acc_h, state_tree, model))
          (parent_y, 0, state_tree, model)
          lst
      in
      (max_width, parent_h, state_tree, model)
  | RowStart lst ->
      let _, _, h, state_tree, model =
        List.fold_left
          (fun (x_pos, acc_w, max_child_h, state_tree, model) el ->
            let max_w = parent_w - acc_w in
            let w, h, state_tree, model =
              draw_widget x_pos parent_y max_w parent_h state_tree model el
            in
            let max_child_h = if h > max_child_h then h else max_child_h in
            (x_pos + w, acc_w + w, max_child_h, state_tree, model))
          (parent_x, 0, 0, state_tree, model)
          lst
      in
      (parent_w, h, state_tree, model)
  | RowCenter lst ->
      row_center_or_end lst (fun parent_w occupied_width ->
          (parent_w / 2) - (occupied_width / 2))
  | RowEnd lst ->
      row_center_or_end lst (fun parent_w occupied_width ->
          parent_w - occupied_width)
  | RowSpaceAround lst ->
      let occupied_width, num_els, max_height =
        List.fold_left
          (fun (acc_size, num_els, max_height) el ->
            let w, h = size parent_w parent_h el in
            (acc_size + w, num_els + 1, if h > max_height then h else max_height))
          (0, 0, 0) lst
      in
      let unoccupied_width = parent_w - occupied_width in
      let hor_gap_size = unoccupied_width / num_els in
      let _, _, state_tree, model =
        List.fold_left
          (fun (x_pos, acc_w, state_tree, model) el ->
            let w, _, state_tree, model =
              draw_widget x_pos parent_y (parent_w - acc_w) parent_h state_tree
                model el
            in
            let x_pos, acc_w =
              (x_pos + w + hor_gap_size, acc_w + w + hor_gap_size)
            in
            (x_pos, acc_w, state_tree, model))
          (parent_x + (hor_gap_size / 2), 0, state_tree, model)
          lst
      in
      (parent_w, max_height, state_tree, model)
  | RowSpaceBetween lst ->
      let occupied_width, num_els, max_height =
        List.fold_left
          (fun (acc_size, num_els, max_height) el ->
            let w, h = size parent_w parent_h el in
            (acc_size + w, num_els + 1, if h > max_height then h else max_height))
          (0, 0, 0) lst
      in
      let unoccupied_width = parent_w - occupied_width in
      let hor_gap_size =
        if num_els <= 1 then 0 else unoccupied_width / (num_els - 1)
      in
      let _, _, state_tree, model =
        List.fold_left
          (fun (x_pos, acc_w, state_tree, model) el ->
            let w, _, state_tree, model =
              draw_widget x_pos parent_y (parent_w - acc_w) max_height
                state_tree model el
            in
            let x_pos, acc_w =
              (x_pos + w + hor_gap_size, acc_w + w + hor_gap_size)
            in
            (x_pos, acc_w, state_tree, model))
          (parent_y, 0, state_tree, model)
          lst
      in
      (parent_w, max_height, state_tree, model)
  | Padding (l, t, r, b, d) ->
      let x_start = parent_x + l in
      let y_start = parent_y + t in
      let max_width = parent_w - (l + r) in
      let max_height = parent_h - (t + b) in
      let c_w, c_h, state_tree, model =
        draw_widget x_start y_start max_width max_height state_tree model d
      in
      (c_w + l + r, c_h + t + b, state_tree, model)
  | Other (f_draw, _, d) ->
      let w, h, state_tree, model =
        f_draw parent_x parent_y parent_w parent_h state_tree model
      in
      let _, _, state_tree, model =
        draw_widget parent_x parent_y w h state_tree model d
      in
      (w, h, state_tree, model)

let draw model view state_tree =
  let width = Raylib.get_screen_width () in
  let height = Raylib.get_screen_height () in
  let _, _, state_tree, model =
    draw_widget 0 0 width height state_tree model view
  in
  (state_tree, model)
