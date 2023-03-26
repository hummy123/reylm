type width = int
type height = int
type radius = float
type left = int
type top = int
type right = int
type bottom = int
type thickness = float
type colour = Raylib.Color.t

type drawable =
  | Column of drawable list
  | Row of drawable list
  | Rect of width * height * radius * colour * drawable
  | Padding of left * top * right * bottom * drawable
  | Border of radius * Raylib.Color.t * thickness * drawable
  | Empty
  | Other of (int -> int -> int -> int -> int * int) * drawable

let rec draw_widget parent_x parent_y parent_w parent_h state_tree = function
  | Empty -> (0, 0, state_tree)
  | Border (r, c, t, d) ->
      let w, h, state_tree =
        draw_widget parent_x parent_y parent_w parent_h state_tree d
      in
      let rect =
        Raylib.Rectangle.create (float_of_int parent_x) (float_of_int parent_y)
          (float_of_int w) (float_of_int h)
      in
      Raylib.draw_rectangle_rounded_lines rect r 10 t c;
      (w, h, state_tree)
  | Rect (w, h, r, c, d) ->
      let w = if w < parent_w then w else parent_w in
      let h = if h < parent_h then h else parent_h in
      let rect =
        Raylib.Rectangle.create (float_of_int parent_x) (float_of_int parent_y)
          (float_of_int w) (float_of_int h)
      in
      Raylib.draw_rectangle_rounded rect r 0 c;
      let _ = draw_widget parent_x parent_y w h state_tree d in
      (w, h, state_tree)
  | Column lst ->
      let _, w, h, state_tree =
        List.fold_left
          (fun (y_pos, max_child_w, acc_h, state_tree) el ->
            let max_w = parent_w in
            let max_h = parent_h - acc_h in
            let w, h, state_tree =
              draw_widget parent_x y_pos max_w max_h state_tree el
            in
            let max_child_w = if w > max_child_w then w else max_child_w in
            (y_pos + h, max_child_w, acc_h + h, state_tree))
          (parent_y, 0, 0, state_tree)
          lst
      in
      (w, h, state_tree)
  | Row lst ->
      let _, w, h, state_tree =
        List.fold_left
          (fun (x_pos, acc_w, max_child_h, state_tree) el ->
            let max_w = parent_w - acc_w in
            let max_h = parent_h in
            let w, h, state_tree =
              draw_widget x_pos parent_y max_w max_h state_tree el
            in
            let max_child_h = if h > max_child_h then h else max_child_h in
            (x_pos + w, acc_w + w, max_child_h, state_tree))
          (parent_x, 0, 0, state_tree)
          lst
      in
      (w, h, state_tree)
  | Padding (l, t, r, b, d) ->
      let x_start = parent_x + l in
      let y_start = parent_y + t in
      let max_width = parent_w - (l + r) in
      let max_height = parent_h - (t + b) in
      let c_w, c_h, state_tree =
        draw_widget x_start y_start max_width max_height state_tree d
      in
      (c_w + l + r, c_h + t + b, state_tree)
  | Other (f, d) ->
      let w, h = f parent_x parent_y parent_w parent_h in
      let _, _, state_tree = draw_widget parent_x parent_y w h state_tree d in
      (w, h, state_tree)

let draw view state_tree =
  (* let state = ref state in *)
  (* let mouse_pos = Raylib.get_mouse_position () in *)
  (* let mouse_x = int_of_float (Raylib.Vector2.x mouse_pos) in *)
  (* let mouse_y = int_of_float (Raylib.Vector2.y mouse_pos) in *)
  (* let did_click = Raylib.is_mouse_button_down Raylib.MouseButton.Left in *)
  let width = Raylib.get_screen_width () in
  let height = Raylib.get_screen_height () in
  let _, _, state_tree = draw_widget 0 0 width height state_tree view in
  state_tree
