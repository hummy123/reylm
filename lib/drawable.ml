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

let size constraints = function
  | Empty -> empty_size
  | Widget (_, f_size) -> f_size constraints
  (* Whether flex is forced to fill or not, return the maximum size.
     This is obvious for when child is forced to fill constraints,
     and when child is not forced to fill, it means there is empty space around the child.
  *)
  | Flex _ -> { width = constraints.max_width; height = constraints.max_height }

let rec draw constraints = function
  | Empty -> empty_size
  | Widget (f_draw, _) -> f_draw constraints
  | Flex (_, fit, child) -> (
      match fit with
      | ForceFill ->
          let constraints =
            {
              constraints with
              min_width = constraints.max_width;
              min_height = constraints.max_height;
            }
          in
          draw constraints child
      | NoForce ->
          let _ = draw constraints child in
          { width = constraints.max_width; height = constraints.max_height })
