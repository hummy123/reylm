open Drawable

val from_ltrb :
  ?left:int ->
  ?top:int ->
  ?right:int ->
  ?bottom:int ->
  'a drawable ->
  'a drawable
(** Adds some space around a child drawable. from_ltrb means "from left, top, right, bottom". *)

val by_axis : ?vertical:int -> ?horizontal:int -> 'a drawable -> 'a drawable
(** Adds some space around a child drawable by specifyig the space on the X-axis and Y-axis. *)

val all : int -> 'a drawable -> 'a drawable
(** Adds some space around a child drawable from all sides. *)
