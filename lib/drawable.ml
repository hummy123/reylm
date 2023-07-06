open Constraints

type flex_fit = Expand | NaturalSize | FillHeight | FillWidth

type 'model drawable =
  | Empty
  | Flex of float * flex_fit * 'model drawable
  | Widget of f_draw * f_size * 'model f_update

(* Whether flex is forced to fill or not, return the maximum size.
   This is obvious for when child is forced to fill constraints.
   When child is not forced to fill, it means there is empty space around the child.
   This matches Flutter's behaviour; see 0:55 here: https://www.youtube.com/watch?v=CI7x0mAZiY0 .
*)
let apply_flex constraints = function
  | Expand ->
      {
        constraints with
        min_width = constraints.max_width;
        min_height = constraints.max_height;
      }
  | FillHeight -> { constraints with min_height = constraints.max_height }
  | FillWidth -> { constraints with min_width = constraints.max_width }
  | NaturalSize -> { constraints with min_width = 0; min_height = 0 }

let rec size constraints = function
  | Empty -> empty_size
  | Widget (_, f_size, _) -> f_size constraints
  | Flex (_, fit, child) ->
      let child_constraints = apply_flex constraints fit in
      size child_constraints child

let rec draw constraints = function
  | Empty -> empty_size
  | Widget (f_draw, _, _) -> f_draw constraints
  | Flex (_, fit, child) ->
      let child_constraints = apply_flex constraints fit in
      draw child_constraints child

let rec update constraints model = function
  | Empty -> { width = 0; height = 0; model }
  | Widget (_, _, f_update) -> f_update constraints model
  | Flex (_, fit, child) ->
      let child_constraints = apply_flex constraints fit in
      update child_constraints model child
