open Drawable

val min : 'a drawable array -> 'a drawable
(** Puts an array of drawables in a column, taking the minimum space those drawables require. *)

val top : ?collapse_width:bool -> 'a drawable array -> 'a drawable
(** Puts an array of drawables in a column at the top of the allowable area. The column takes the maximum height it possibly can. *)

val center : ?collapse_width:bool -> 'a drawable array -> 'a drawable
(** Puts an array of drawables in a column at the vertical center of the allowable area. The column takes the maximum height it possibly can. *)

val bottom : ?collapse_width:bool -> 'a drawable array -> 'a drawable
(** Puts an array of drawables in a column at the bottom of the allowable area. The column takes the maximum height it possibly can. *)

val space_between : ?collapse_width:bool -> 'a drawable array -> 'a drawable
(** Puts an array of drawables in a column with an equal space between each drawable. *)

val space_around : ?collapse_width:bool -> 'a drawable array -> 'a drawable
(** Puts an array of drawables in a column with an equal space between each drawable. 
    The topmost and bottom-most drawables have a half-space between them and the edge. *)
