type width = int
type height = int
type radius = float
type left = int
type top = int
type right = int
type bottom = int
type thickness = float
type colour = Raylib.Color.t
type vert_align = Top | Middle | Bottom
type hor_align = Left | Middle | Right

(* Load font. *)

(* 'a is type of the domain model for the user's app.' *)
type 'a drawable =
  | Column of 'a drawable list
  | Text of string * float
  | Row of 'a drawable list
  | Rect of width * height * radius * colour * 'a drawable
  | Padding of left * top * right * bottom * 'a drawable
  | Border of radius * Raylib.Color.t * thickness * 'a drawable
  | Empty
  | Align of vert_align * hor_align * 'a drawable
  | Other of
      (int ->
      int ->
      int ->
      int ->
      State_tree.state_tree ->
      'a ->
      int * int * State_tree.state_tree * 'a)
      * (int -> int -> 'a drawable -> int * int)
      * 'a drawable

let rec size parent_w parent_h = function
  | Empty -> (0, 0)
  | Border (_, _, _, d) -> size parent_w parent_h d
  | Rect (w, h, _, _, _) ->
      let w = if w < parent_w then w else parent_w in
      let h = if h < parent_h then h else parent_h in
      (w, h)
  | Column lst ->
      let max_w =
        List.fold_left
          (fun max_w el ->
            let c_w, _ = size parent_w parent_h el in
            let max_w = if c_w > max_w then c_w else max_w in
            max_w)
          0 lst
      in
      (max_w, parent_h)
  | Row lst ->
      let max_h =
        List.fold_left
          (fun (max_h : int) el ->
            let _, c_h = size parent_w parent_h el in
            let max_h = if c_h > max_h then c_h else max_h in
            max_h)
          0 lst
      in
      (parent_w, max_h)
  | Text(str, font_size) ->
      let font = Raylib.load_font "resources/Noto_Sans/NotoSans-Regular.ttf" in
      let s = Raylib.measure_text_ex (font) str font_size 3.0 in
      let w = Raylib.Vector2.x s in
      let h = Raylib.Vector2.y s in
      (int_of_float w, int_of_float h)
  | Padding _ -> (parent_w, parent_h)
  | Align (_, _, d) -> size parent_w parent_h d
  | Other (_, f_calc, d) -> f_calc parent_w parent_h d

let rec draw_widget parent_x parent_y parent_w parent_h state_tree model =
  function
  | Empty -> (0, 0, state_tree, model)
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
  | Column lst ->
      let _, w, _, state_tree, model =
        List.fold_left
          (fun (y_pos, max_child_w, acc_h, state_tree, model) el ->
            let max_w = parent_w in
            let max_h = parent_h - acc_h in
            let w, h, state_tree, model =
              draw_widget parent_x y_pos max_w max_h state_tree model el
            in
            let max_child_w = if w > max_child_w then w else max_child_w in
            (y_pos + h, max_child_w, acc_h + h, state_tree, model))
          (parent_y, 0, 0, state_tree, model)
          lst
      in
      (w, parent_h, state_tree, model)
  | Row lst ->
      let _, _, h, state_tree, model =
        List.fold_left
          (fun (x_pos, acc_w, max_child_h, state_tree, model) el ->
            let max_w = parent_w - acc_w in
            let max_h = parent_h in
            let w, h, state_tree, model =
              draw_widget x_pos parent_y max_w max_h state_tree model el
            in
            let max_child_h = if h > max_child_h then h else max_child_h in
            (x_pos + w, acc_w + w, max_child_h, state_tree, model))
          (parent_x, 0, 0, state_tree, model)
          lst
      in
      (parent_w, h, state_tree, model)
  | Text(str, font_size) ->
      let pos = Raylib.Vector2.create (float_of_int parent_x) (float_of_int parent_y) in
      let font = Raylib.load_font "resources/Noto_Sans/NotoSans-Regular.ttf" in
      Raylib.draw_text_ex font str pos font_size 3.0  Raylib.Color.black;
      let size = Raylib.measure_text_ex font str font_size 204.0 in
      let w = Raylib.Vector2.x size |> int_of_float in
      let h = Raylib.Vector2.y size |> int_of_float in
      (w, h, state_tree, model)
  | Padding (l, t, r, b, d) ->
      let x_start = parent_x + l in
      let y_start = parent_y + t in
      let max_width = parent_w - (l + r) in
      let max_height = parent_h - (t + b) in
      let c_w, c_h, state_tree, model =
        draw_widget x_start y_start max_width max_height state_tree model d
      in
      (c_w + l + r, c_h + t + b, state_tree, model)
  | Align (v, h, d) ->
      let c_w, c_h = size parent_w parent_h d in
      let x_pos =
        match h with
        | Left -> parent_x
        | Middle ->
            let end_x = parent_x + parent_w in
            let mid_x = (parent_x + end_x) / 2 in
            mid_x - (c_w / 2)
        | Right ->
            let end_x = parent_x + parent_w in
            end_x - c_w
      in
      let y_pos =
        match v with
        | Top -> parent_y
        | Middle ->
            let end_y = parent_y + parent_h in
            let mid_y = (parent_y + end_y) / 2 in
            mid_y - (c_h / 2)
        | Bottom ->
            let end_y = parent_y + parent_h in
            end_y - c_h
      in
      draw_widget x_pos y_pos parent_w parent_h state_tree model d
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