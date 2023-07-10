open Drawable

val widget : ?x_shift:float -> ?y_shift:float -> 'a drawable -> 'a drawable
(** 
    Positions a child left or right within its constraints. 
    x_shift moves the child along the x axis and y_shift moves the child along the y axis.
    The default for both is 0.0 at the centre and the allowable range is from -1.0 to 1.0.
    An x_shift value of -1.0 describes a child at the left and a y_shift value of -1.0 describes a child at the top.
  *)
