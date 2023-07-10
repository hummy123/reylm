open Drawable

val exec : bool -> ('a -> 'a) -> 'a drawable
(** Updates the model with the provided function whenever bool is true. *)
