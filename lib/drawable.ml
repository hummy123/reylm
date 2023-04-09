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
type drawable = Empty | Widget of (input_constraints -> drawn_size)

let draw constraints = function
  | Empty -> { width = 0; height = 0 }
  | Widget f_draw -> f_draw constraints
