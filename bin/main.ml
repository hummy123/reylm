type drawable = Column of drawable list * Raylib.Color.t | Rectangle of int * int * Raylib.Color.t | Row of drawable list * Raylib.Color.t

let placeholder = 
  Row ([
    Column ([ Rectangle (20, 200, Raylib.Color.red); Rectangle (100, 400, Raylib.Color.beige) ], Raylib.Color.brown);
    Column ([ Rectangle (50, 300, Raylib.Color.darkgreen); Rectangle (200, 200, Raylib.Color.orange) ], Raylib.Color.darkbrown);
  ], Raylib.Color.gold)

let rec calc parent_w parent_h = function
  | Rectangle (w, h, c) ->
      let w = if w < parent_w then w else parent_w in
      let h = if h < parent_h then h else parent_h in
      (w, h, c)
  | Column(lst, c) ->
      let length = List.length lst in
      let max_w = parent_w / length in
      let max_h = parent_h / length in
      let _ =
        List.fold_left
          (fun y_pos el ->
            let w, h, c = calc max_h max_w el in
            Raylib.draw_rectangle 0 y_pos w h c;
            y_pos + h)
          0 lst
      in
      (max_w, max_h, c)
  | Row(lst, c) ->
      let length = List.length lst in
      let max_w = parent_w / length in
      let max_h = parent_h / length in
      let _ =
        List.fold_left
          (fun x_pos el ->
            let w, h, c = calc max_h max_w el in
            Raylib.draw_rectangle x_pos 0 w h c;
            x_pos + w)
          0 lst
      in
      max_w, max_h, c

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
      let _ = calc height width placeholder in
      clear_background Color.raywhite;
      draw_text "Congrats! You created your first window!" 190 200 20
        Color.lightgray;
      end_drawing ();
      loop ()

let () = setup () |> loop
