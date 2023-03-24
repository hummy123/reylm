type width = int
type height = int
type radius = float
type left = int
type top = int
type right = int
type bottom = int
type thickness = float
type label = string

type drawable =
  | Column of drawable list
  | Row of drawable list
  | Button of label * width * height * radius * Raylib.Color.t * drawable
  | Padding of left * top * right * bottom * drawable
  | Box of Raylib.Color.t * drawable
  | Border of radius * Raylib.Color.t * thickness * drawable
  | Empty

let rec draw parent_x parent_y parent_w parent_h = function
  | Empty -> (0, 0)
  | Box (c, d) ->
      let w, h = draw parent_x parent_y parent_w parent_h d in
      Raylib.draw_rectangle parent_x parent_y w h c;
      let _ = draw parent_x parent_y parent_w parent_h d in
      (w, h)
  | Border (r, c, t, d) ->
      let w, h = draw parent_x parent_y parent_w parent_h d in
      let rect =
        Raylib.Rectangle.create (float_of_int parent_x) (float_of_int parent_y)
          (float_of_int w) (float_of_int h)
      in
      Raylib.draw_rectangle_rounded_lines rect r 10 t c;
      (w, h)
  | Button (_, w, h, r, c, d) ->
      let w = if w < parent_w then w else parent_w in
      let h = if h < parent_h then h else parent_h in
      let rect =
        Raylib.Rectangle.create (float_of_int parent_x) (float_of_int parent_y)
          (float_of_int w) (float_of_int h)
      in
      Raylib.draw_rectangle_rounded rect r 0 c;
      let _ = draw parent_x parent_y w h d in
      (w, h)
  | Column lst ->
      let _, w, h =
        List.fold_left
          (fun (y_pos, max_child_w, acc_h) el ->
            let max_w = parent_w in
            let max_h = parent_h - acc_h in
            let w, h = draw parent_x y_pos max_w max_h el in
            let max_child_w = if w > max_child_w then w else max_child_w in
            (y_pos + h, max_child_w, acc_h + h))
          (parent_y, 0, 0) lst
      in
      (w, h)
  | Row lst ->
      let _, w, h =
        List.fold_left
          (fun (x_pos, acc_w, max_child_h) el ->
            let max_w = parent_w - acc_w in
            let max_h = parent_h in
            let w, h = draw x_pos parent_y max_w max_h el in
            let max_child_h = if h > max_child_h then h else max_child_h in
            (x_pos + w, acc_w + w, max_child_h))
          (parent_x, 0, 0) lst
      in
      (w, h)
  | Padding (l, t, r, b, d) ->
      let x_start = parent_x + l in
      let y_start = parent_y + t in
      let max_width = parent_w - (l + r) in
      let max_height = parent_h - (t + b) in
      let c_w, c_h = draw x_start y_start max_width max_height d in
      (c_w + l + r, c_h + t + b)
