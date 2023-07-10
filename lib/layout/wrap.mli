open Drawable

val row_min : ?row_padding:int -> 'a drawable array -> 'a drawable
(** Like {!Row.min}, but if the children don't entirely fit into a single row,
    a new row is inserted below. *)

val row_left : ?row_padding:int -> 'a drawable array -> 'a drawable
(** Like {!Row.left}, but if the children don't entirely fit into a single row,
    a new row is inserted below. *)

val row_right : ?row_padding:int -> 'a drawable array -> 'a drawable
(** Like {!Row.right}, but if the children don't entirely fit into a single row,
    a new row is inserted below. *)

val row_space_between : ?row_padding:int -> 'a drawable array -> 'a drawable
(** Like {!Row.space_between}, but if the children don't entirely fit into a single row,
    a new row is inserted below. *)

val row_space_around : ?row_padding:int -> 'a drawable array -> 'a drawable
(** Like {!Row.space_around}, but if the children don't entirely fit into a single row,
    a new row is inserted below. *)

val column_min : ?col_padding:int -> 'a drawable array -> 'a drawable
(** Like {!Column.min}, but if the children don't entirely fit into a single column,
    a new column is inserted on the right. *)

val column_top : ?col_padding:int -> 'a drawable array -> 'a drawable
(** Like {!Column.top}, but if the children don't entirely fit into a single column,
    a new column is inserted on the right. *)

val column_bottom : ?col_padding:int -> 'a drawable array -> 'a drawable
(** Like {!Column.bottom}, but if the children don't entirely fit into a single column,
    a new column is inserted on the right. *)

val column_space_between : ?col_padding:int -> 'a drawable array -> 'a drawable
(** Like {!Column.space_between}, but if the children don't entirely fit into a single column,
    a new column is inserted on the right. *)

val column_space_around : ?col_padding:int -> 'a drawable array -> 'a drawable
(** Like {!Column.space_around}, but if the children don't entirely fit into a single column,
    a new column is inserted on the right. *)
