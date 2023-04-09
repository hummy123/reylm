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

type drawable_size = { width : int; height : int }

type drawable =
  | Empty
  | Widget of
      (input_constraints -> drawable_size)
      * (input_constraints -> drawable_size)

let empty_size = { width = 0; height = 0 }

let size contraints = function
  | Empty -> empty_size
  | Widget (_, f_size) -> f_size contraints

let draw constraints = function
  | Empty -> empty_size
  | Widget (f_draw, _) -> f_draw constraints
