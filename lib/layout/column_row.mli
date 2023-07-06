open Constraints
open Flex
open Drawable

type caller = Row | Column

val flex_draw :
  caller -> flex_data -> 'a drawable array -> input_constraints -> drawable_size

val flex_update :
  caller ->
  flex_data ->
  'a drawable array ->
  input_constraints ->
  'a ->
  'a drawable_output

val min_size : 'a drawable array -> caller -> input_constraints -> drawable_size
val min_draw : 'a drawable array -> caller -> input_constraints -> drawable_size

val min_update :
  'a drawable array -> caller -> input_constraints -> 'a -> 'a drawable_output

val max_size :
  bool -> 'a drawable array -> caller -> input_constraints -> drawable_size

val directional_draw :
  (input_constraints -> flex_data -> int) ->
  bool ->
  'a drawable array ->
  caller ->
  input_constraints ->
  drawable_size

val directional_update :
  (input_constraints -> flex_data -> int) ->
  bool ->
  'a drawable array ->
  caller ->
  input_constraints ->
  'a ->
  'a drawable_output

val calc_when_start : 'a -> 'b -> int
val calc_when_center : caller -> input_constraints -> flex_data -> int
val calc_when_end : caller -> input_constraints -> flex_data -> int

(* val draw_space_between: bool -> 'a drawable array -> caller -> input_constraints -> drawable_size *)
(* val update_space_between: bool -> 'a drawable array -> caller -> input_constraints -> 'a -> 'a drawable_output *)

val draw_space_around :
  bool -> 'a drawable array -> caller -> input_constraints -> drawable_size

val update_space_around :
  bool ->
  'a drawable array ->
  caller ->
  input_constraints ->
  'a ->
  'a drawable_output
