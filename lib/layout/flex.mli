open Drawable

val expand : ?flex_val:float -> 'a drawable -> 'a drawable
(** Forces the child drawable to take the maximum width and height it possibly can.
    If multiple Flex drawables are in a non-min {!Column} or {!Row}, 
    they divide the remaining space between themselves as in CSS and Flutter. *)

val fill_height : ?flex_val:float -> 'a drawable -> 'a drawable
(** Forces the child drawable to take the maximum height it possibly can.
    If multiple Flex drawables are in a non-min {!Column} or {!Row}, 
    they divide the remaining space between themselves as in CSS and Flutter. *)

val fill_width : ?flex_val:float -> 'a drawable -> 'a drawable
(** Forces the child drawable to take the maximum width it possibly can.
    If multiple Flex drawables are in a non-min {!Column} or {!Row}, 
    they divide the remaining space between themselves as in CSS and Flutter. *)

val natural_size : ?flex_val:float -> 'a drawable -> 'a drawable
(** Allows the child widget to be as large as it wants (within the constraints of other widgets)
    or as small as it wants. *)
