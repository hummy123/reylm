open Raylib

type input_constraints = {
  (* The x coordinate this drawable should start drawing at. *)
  start_x : int;
  (* The y coordinate this drawable should start drawing at. *)
  start_y : int;
  (* The total width of this drawable's parent. This drawable should not go beyond this width. *)
  max_width : int;
  min_width : int;
  (* The total height of this drawable's parent. This drawable should not go beyond this height. *)
  max_height : int;
  min_height : int;
}

type drawn_size = { width : int; height : int }

type drawable =
  | Rect of int * int * float * Color.t
  | Widget of (input_constraints -> drawn_size)

let draw constraints = function
  | Rect (width, height, radius, colour) ->
      let width =
        if constraints.min_width > width then constraints.min_width
        else if constraints.max_width < width then constraints.max_width
        else width
      in
      let height =
        if constraints.min_height > height then constraints.min_height
        else if constraints.max_height < height then constraints.max_height
        else height
      in
      let f_width = float_of_int width in
      let f_height = float_of_int height in
      let x = float_of_int constraints.start_x in
      let y = float_of_int constraints.start_y in
      let rect = Rectangle.create x y f_width f_height in
      Raylib.draw_rectangle_rounded rect radius 0 colour;
      { width; height }
  | Widget _ -> { width = 0; height = 0 }
