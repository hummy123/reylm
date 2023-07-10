open Drawable

val widget : ?width:float -> ?height:float -> 'a drawable -> 'a drawable
(** 
    Creates an empty box that takes a child, and can optionally take width and height values.
    The width and height values can be from 0.0 to 1.0. 
    The given value means "take this percentage of the allowable width/height" where 0.0 is 0% and 1.0 is 100%.
 *)
