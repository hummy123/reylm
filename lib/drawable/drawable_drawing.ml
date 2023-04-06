open Drawable_types
open Drawable_sizes

let rec draw_widget (input : 'a Drawable_types.draw_widget_input) = function
  | Empty ->
      {
        width = 0;
        height = 0;
        state_tree = input.state_tree;
        model = input.model;
      }
  | HLine col ->
      Raylib.draw_line input.parent_x input.parent_y
        (input.parent_x + input.parent_w)
        input.parent_y col;
      {
        width = input.parent_w;
        height = 1;
        state_tree = input.state_tree;
        model = input.model;
      }
  | VLine col ->
      Raylib.draw_line input.parent_x input.parent_y input.parent_x
        (input.parent_y + input.parent_h)
        col;
      {
        width = 1;
        height = input.parent_h;
        state_tree = input.state_tree;
        model = input.model;
      }
  | Border (r, c, t, d) ->
      let output = draw_widget input d in
      let rect =
        Raylib.Rectangle.create
          (float_of_int input.parent_x)
          (float_of_int input.parent_y)
          (float_of_int output.width)
          (float_of_int output.height)
      in
      Raylib.draw_rectangle_rounded_lines rect r 10 t c;
      output
  | Overlay d ->
      let width, height, state_tree, model =
        List.fold_left
          (fun (max_w, max_h, state_tree, model) el ->
            let input =
              {
                parent_x = input.parent_x;
                parent_y = input.parent_y;
                parent_w = input.parent_w;
                parent_h = input.parent_h;
                state_tree;
                model;
              }
            in
            let output = draw_widget input el in
            (output.width, output.height, output.state_tree, output.model))
          (0, 0, input.state_tree, input.model)
          d
      in
      { width; height; state_tree; model }
  | Rect (width, height, r, c, d) ->
      let rect =
        Raylib.Rectangle.create
          (float_of_int input.parent_x)
          (float_of_int input.parent_y)
          (float_of_int width) (float_of_int height)
      in
      let input = { input with parent_w = width; parent_h = height } in
      Raylib.draw_rectangle_rounded rect r 0 c;
      let output = draw_widget input d in
      { width; height; state_tree = output.state_tree; model = output.model }
  | VPanel lst ->
      let _, width, height, state_tree, model =
        List.fold_left
          (fun (y_pos, max_w, acc_h, state_tree, model) el ->
            let max_h = input.parent_h - acc_h in
            let input =
              {
                parent_x = input.parent_x;
                parent_y = y_pos;
                parent_w = input.parent_w;
                parent_h = max_h;
                state_tree;
                model;
              }
            in
            let output = draw_widget input el in
            let max_w = if output.width > max_w then output.width else max_w in
            let acc_h = acc_h + output.height in
            ( y_pos + output.height,
              max_w,
              acc_h,
              output.state_tree,
              output.model ))
          (0, 0, 0, input.state_tree, input.model)
          lst
      in
      { width; height; state_tree; model }
  | HPanel lst ->
      let _, width, height, state_tree, model =
        List.fold_left
          (fun (x_pos, acc_w, max_h, state_tree, model) el ->
            let max_child_w = input.parent_w - acc_w in
            let input =
              {
                parent_x = x_pos;
                parent_y = input.parent_y;
                parent_w = max_child_w;
                parent_h = input.parent_h;
                state_tree;
                model;
              }
            in
            let output = draw_widget input el in
            let max_h =
              if output.height > max_h then output.height else max_h
            in
            let acc_w = acc_w + output.width in
            (x_pos + output.width, acc_w, max_h, output.state_tree, output.model))
          (0, 0, 0, input.state_tree, input.model)
          lst
      in
      { width; height; state_tree; model }
  | ColumnStart lst -> Column.draw_start draw_widget input lst
  | ColumnCenter lst -> Column.draw_center draw_widget input lst
  | ColumnEnd lst -> Column.draw_end draw_widget input lst
  | ColumnSpaceAround lst -> Column.draw_space_around draw_widget input lst
  | ColumnSpaceBetween lst -> Column.draw_space_between draw_widget input lst
  | RowStart lst -> Row.draw_start draw_widget input lst
  | RowCenter lst -> Row.draw_center draw_widget input lst
  | RowEnd lst -> Row.draw_end draw_widget input lst
  | RowSpaceAround lst -> Row.draw_space_around draw_widget input lst
  | RowSpaceBetween lst -> Row.draw_space_between draw_widget input lst
  | Padding (l, t, r, b, d) ->
      let x_start = input.parent_x + l in
      let y_start = input.parent_y + t in
      let max_width = input.parent_w - (l + r) in
      let max_height = input.parent_h - (t + b) in
      let input =
        {
          parent_x = x_start;
          parent_y = y_start;
          parent_w = max_width;
          parent_h = max_height;
          state_tree = input.state_tree;
          model = input.model;
        }
      in
      let output = draw_widget input d in
      {
        output with
        width = output.width + l + r;
        height = output.height + t + b;
      }
  | Other (f_draw, _, d) ->
      let output = f_draw input in
      let input =
        {
          parent_x = input.parent_x;
          parent_y = input.parent_y;
          parent_w = output.width;
          parent_h = output.height;
          state_tree = output.state_tree;
          model = output.model;
        }
      in
      let child_output = draw_widget input d in
      { child_output with width = output.width; height = output.height }

let draw view input = draw_widget input view
