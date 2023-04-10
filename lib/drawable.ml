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
type flex_fit = ForceFill | NoForce

type drawable =
  | Empty
  | Flex of int * flex_fit * drawable
  | Widget of
      (input_constraints -> drawable_size)
      * (input_constraints -> drawable_size)

let empty_size = { width = 0; height = 0 }

let rec size constraints = function
  | Empty -> empty_size
  | Widget (_, f_size) -> f_size constraints
  (* Reminder: We should never call Drawable.size on a Row/Column or any Widget that accepts multiple children
     before checking total flex of all children. *)
  | Flex (_, fit, child) -> (
      match fit with
      | ForceFill ->
          { width = constraints.max_width; height = constraints.max_height }
      | NoForce -> size constraints child)

let rec draw constraints = function
  | Empty -> empty_size
  | Widget (f_draw, _) -> f_draw constraints
  | Flex (_, fit, child) -> (
      match fit with
      | ForceFill ->
          let constraints =
            {
              constraints with
              min_width = constraints.max_height;
              min_height = constraints.max_height;
            }
          in
          draw constraints child
      | NoForce -> draw constraints child)
