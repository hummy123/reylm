open Constraints
open Drawable
open Flex

let min children =
  Widget
    ( Column_row.min_draw children Row,
      Column_row.min_size children Row,
      Column_row.min_update children Row )

let left ?(collapse_height = true) children =
  Widget
    ( Column_row.directional_draw Column_row.calc_when_start collapse_height
        children Row,
      Column_row.max_size collapse_height children Row,
      Column_row.directional_update Column_row.calc_when_start collapse_height
        children Row )

let center ?(collapse_height = true) children =
  Widget
    ( Column_row.directional_draw
        (Column_row.calc_when_center Row)
        collapse_height children Row,
      Column_row.max_size collapse_height children Row,
      Column_row.directional_update
        (Column_row.calc_when_center Row)
        collapse_height children Row )

(* Functions for drawing row aligned to right. *)
let calc_start_x_right constraints flex_data =
  constraints.max_width - flex_data.occupied_non_flex_width

let right ?(collapse_height = true) children =
  Widget
    ( Column_row.directional_draw
        (Column_row.calc_when_end Row)
        collapse_height children Row,
      Column_row.max_size collapse_height children Row,
      Column_row.directional_update
        (Column_row.calc_when_end Row)
        collapse_height children Row )

(* Functions for drawing row with space in between/around. *)

(* let space_between ?(collapse_height = true) children = *)
(* Widget *)
(*   ( Column_row.draw_space_between collapse_height children Row, *)
(*     Column_row.max_size collapse_height children Row ) *)

let space_around ?(collapse_height = true) children =
  Widget
    ( Column_row.draw_space_around collapse_height children Row,
      Column_row.max_size collapse_height children Row,
      Column_row.update_space_around collapse_height children Row )
