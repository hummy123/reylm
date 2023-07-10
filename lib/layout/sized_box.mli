open Drawable

val widget : width:int -> height:int -> 'a drawable -> 'a drawable
(** 
    Creates an empty box that takes a child, and can optionally take width and height values.
    The width and height values represent the pixels taken and will automatically be clipped if they exceed the allowable area.
 *)
