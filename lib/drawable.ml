(* Types and functions for just simple drawing (rendering control). *)

(* Constraints tell a drawable how much height and width it can have and the coordinates to start drawing from. *)
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

(* Same as input_constraints, but passes model state. *)
type 'a state_constraints = {
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
  model : 'a;
}

(* drawable_size tells the parent how much height and width the child drawable actually took up,
   which the parent can use to determine positioning. *)
type drawable_size = { width : int; height : int }

let empty_size = { width = 0; height = 0 }

(* Same as drawable_size, but returns updated model as well. *)
type 'a model_output = { width : int; height : int; model : 'a }

(* Different flex options. *)
type flex_fit = Expand | NaturalSize | FillHeight | FillWidth

(* Type abbreviations of what the drawables have to implement. *)
type f_draw = input_constraints -> drawable_size
type f_size = input_constraints -> drawable_size
type 'a f_model = 'a state_constraints -> 'a model_output

(* Widgets have to implemented through one of these constructors. *)
type 'a drawable =
  | Empty
  | Flex of float * flex_fit * 'a drawable
  | Widget of f_draw * f_size * 'a f_model

(* Row/column preprocessing data for calculating flex values. *)
type flex_data = {
  total_flex_height : float;
  total_flex_width : float;
  occupied_non_flex_height : int;
  occupied_non_flex_width : int;
  num_flex_width_children : int;
  num_flex_height_children : int;
  max_child_height : int;
  max_child_width : int;
}

let initial_flex_data =
  {
    total_flex_height = 0.;
    total_flex_width = 0.;
    num_flex_height_children = 0;
    num_flex_width_children = 0;
    occupied_non_flex_height = 0;
    occupied_non_flex_width = 0;
    max_child_height = 0;
    max_child_width = 0;
  }

(* Two identical functions, apply_flex_draw and apply_flex_update except one updates input_constraints and one updates state_constraints.
   Controls whether flex is forced to fill or not, return the maximum size.
   This is obvious for when child is forced to fill constraints.
   When child is not forced to fill, it means there is empty space around the child.
   This matches Flutter's behaviour; see 0:55 here: https://www.youtube.com/watch?v=CI7x0mAZiY0 .
*)
let apply_flex_draw (constraints : input_constraints) = function
  | Expand ->
      {
        constraints with
        min_width = constraints.max_width;
        min_height = constraints.max_height;
      }
  | FillHeight -> { constraints with min_height = constraints.max_height }
  | FillWidth -> { constraints with min_width = constraints.max_width }
  | NaturalSize -> { constraints with min_width = 0; min_height = 0 }

let apply_flex_update constraints = function
  | Expand ->
      {
        constraints with
        min_width = constraints.max_width;
        min_height = constraints.max_height;
      }
  | FillHeight -> { constraints with min_height = constraints.max_height }
  | FillWidth -> { constraints with min_width = constraints.max_width }
  | NaturalSize -> { constraints with min_width = 0; min_height = 0 }

(* Function for retrieving size of child. *)
let rec size constraints = function
  | Empty -> empty_size
  | Widget (_, f_size, _) -> f_size constraints
  | Flex (_, fit, child) -> size (apply_flex_draw constraints fit) child

(* Drawing function. *)
let rec draw constraints = function
  | Empty -> empty_size
  | Widget (f_draw, _, _) -> f_draw constraints
  | Flex (_, fit, child) -> draw (apply_flex_draw constraints fit) child

(* Function for updating model. *)
let rec update_model (constraints : 'a state_constraints) = function
  | Empty -> { width = 0; height = 0; model = constraints.model }
  | Widget (_, _, f_update_model) -> f_update_model constraints
  | Flex (_, fit, child) ->
      update_model (apply_flex_update constraints fit) child
