open Drawable

val from_ltrb :
  ?left:int ->
  ?top:int ->
  ?right:int ->
  ?bottom:int ->
  'a drawable ->
  'a drawable

val by_axis : ?vertical:int -> ?horizontal:int -> 'a drawable -> 'a drawable
val all : int -> 'a drawable -> 'a drawable
