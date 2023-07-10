open Drawable

val min : 'a drawable array -> 'a drawable
(** Puts an array of drawables in a row, taking the minimum space those drawables require. *)

val left : ?collapse_height:bool -> 'a drawable array -> 'a drawable
(** Puts an array of drawables in a row to the leftmost part of the allowable area. The row takes the maximum width it possibly can. *)

val center : ?collapse_height:bool -> 'a drawable array -> 'a drawable
(** Puts an array of drawables in a row at the horizontal center of the allowable area. The row takes the maximum width it possibly can. *)

val right : ?collapse_height:bool -> 'a drawable array -> 'a drawable
(** Puts an array of drawables in a row to the rightmost part of the allowable area. The row takes the maximum width it possibly can. *)

val space_around : ?collapse_height:bool -> 'a drawable array -> 'a drawable
(** Puts an array of drawables in a row with an equal space between each drawable. *)

val space_between : ?collapse_height:bool -> 'a drawable array -> 'a drawable
(** Puts an array of drawables in a row with an equal space between each drawable. 
    The leftmost and rightmost drawables have a half-space between them and the edge. *)
