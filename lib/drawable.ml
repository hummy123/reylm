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
type flex_fit = Expand | NaturalSize | FillHeight | FillWidth

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
     This matches Flutter's behaviour; see 0:55 here: https://www.youtube.com/watch?v=CI7x0mAZiY0 .
  *)
  | Flex _ -> { width = constraints.max_width; height = constraints.max_height }

let rec draw constraints = function
  | Empty -> empty_size
  | Widget (f_draw, _) -> f_draw constraints
  | Flex (_, fit, child) ->
      let child_constraints =
        match fit with
        | Expand ->
            {
              constraints with
              min_width = constraints.max_width;
              min_height = constraints.max_height;
            }
        | FillHeight -> { constraints with min_height = constraints.max_height }
        | FillWidth -> { constraints with min_width = constraints.max_width }
        | NaturalSize -> constraints
      in
      let _ = draw child_constraints child in
      { width = constraints.max_width; height = constraints.max_height }
