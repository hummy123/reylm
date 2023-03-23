type drawable =
  | Column of drawable list
  | Row of drawable list
  | Rectangle of int * int * Raylib.Color.t
  | Padding of int * int * drawable

let placeholder =
  Row
    [
      Column
        [
          Rectangle (20, 200, Raylib.Color.red);
          Padding (30, 30, Rectangle (100, 400, Raylib.Color.beige));
        ];
      Column
        [
          Rectangle (50, 300, Raylib.Color.darkgreen);
          Rectangle (200, 200, Raylib.Color.orange);
        ];
    ]

let rec calc parent_x parent_y parent_w parent_h = function
  | Rectangle (w, h, c) ->
      let w = if w < parent_w then w else parent_w in
      let h = if h < parent_h then h else parent_h in
      Raylib.draw_rectangle parent_x parent_y w h c;
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
  | Padding (w, h, d) ->
      let x_offset = parent_x + (w / 2) in
      let y_offset = parent_y + (h / 2) in
      let max_width = parent_w - (w * 2) in
      let max_height = parent_h - (h * 2) in
      let c_w, c_h = calc x_offset y_offset max_width max_height d in
      (c_w + w, c_h + h)

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
