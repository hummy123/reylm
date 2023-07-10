open Drawable

val row_min : ?row_padding:int -> 'a drawable array -> 'a drawable
val row_left : ?row_padding:int -> 'a drawable array -> 'a drawable
val row_right : ?row_padding:int -> 'a drawable array -> 'a drawable
val row_space_between : ?row_padding:int -> 'a drawable array -> 'a drawable
val row_space_around : ?row_padding:int -> 'a drawable array -> 'a drawable
val column_min : ?col_padding:int -> 'a drawable array -> 'a drawable
val column_top : ?col_padding:int -> 'a drawable array -> 'a drawable
val column_bottom : ?col_padding:int -> 'a drawable array -> 'a drawable
val column_space_between : ?col_padding:int -> 'a drawable array -> 'a drawable
val column_space_around : ?col_padding:int -> 'a drawable array -> 'a drawable
