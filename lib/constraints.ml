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

(* Converts state_constraints to input_constraints.
   Helpful because sometimes we want size of child  but don't want to write boilerplate converting
   between both types. *)
let drop_model (state_constraints : 'a state_constraints) =
  let { start_x; start_y; max_width; min_width; max_height; min_height; _ } =
    state_constraints
  in
  { start_x; start_y; max_width; min_width; max_height; min_height }

(* drawable_size tells the parent how much height and width the child drawable actually took up,
   which the parent can use to determine positioning. *)
type drawable_size = { width : int; height : int }

let empty_size = { width = 0; height = 0 }

(* Same as drawable_size, but returns updated model as well. *)
type 'a model_output = { width : int; height : int; model : 'a }
