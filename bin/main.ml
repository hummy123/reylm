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

let placeholder =
  Row
    [
      Box
        ( Raylib.Color.black,
          Column
            [
              Button ("", 20, 200, 0.0, Raylib.Color.red, Empty);
              Padding
                ( 10,
                  20,
                  30,
                  40,
                  Button ("", 100, 400, 0.2, Raylib.Color.beige, Empty) );
            ] );
      Column
        [
          Border
            ( 10.0,
              Raylib.Color.raywhite,
              4.0,
              Button ("", 50, 300, 0.4, Raylib.Color.darkgreen, Empty) );
          Border
            ( 0.2,
              Raylib.Color.pink,
              1.0,
              Button ("", 200, 200, 0.2, Raylib.Color.orange, Empty) );
        ];
    ]

let rec calc parent_x parent_y parent_w parent_h = function
  | Empty -> (0, 0)
  | Box (c, d) ->
      let w, h = calc parent_x parent_y parent_w parent_h d in
      Raylib.draw_rectangle parent_x parent_y w h c;
      let _ = calc parent_x parent_y parent_w parent_h d in
      (w, h)
  | Border (r, c, t, d) ->
      let w, h = calc parent_x parent_y parent_w parent_h d in
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
      let _ = calc parent_x parent_y w h d in
      (w, h)
  | Column lst ->
      let _, w, h =
        List.fold_left
          (fun (y_pos, max_child_w, acc_h) el ->
            let max_w = parent_w in
            let max_h = parent_h - acc_h in
            let w, h = calc parent_x y_pos max_w max_h el in
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
            let w, h = calc x_pos parent_y max_w max_h el in
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
      let c_w, c_h = calc x_start y_start max_width max_height d in
      (c_w + l + r, c_h + t + b)

let setup () =
  let width = Raylib.get_monitor_width 0 in
  let height = Raylib.get_monitor_height 0 in
  Raylib.set_config_flags [ Window_resizable; Vsync_hint ];
  Raylib.init_window width height "raylib [core] example - basic window";
  ()

let rec loop () =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let height = Raylib.get_screen_height () in
      let width = Raylib.get_screen_width () in
      let open Raylib in
      begin_drawing ();
      let _ = calc 0 0 width height placeholder in
      clear_background Color.raywhite;
      draw_text "Congrats! You created your first window!" 190 200 20
        Color.lightgray;
      end_drawing ();
      loop ()

let () = setup () |> loop
