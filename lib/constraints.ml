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

let empty_size = { width = 0; height = 0 }

type 'model drawable_output = { width : int; height : int; model : 'model }
type f_draw = input_constraints -> drawable_size
type f_size = input_constraints -> drawable_size
type 'model f_update = input_constraints -> 'model -> 'model drawable_output
