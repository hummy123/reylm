open Drawable

let min children =
  Widget
    ( Column_row.min_draw children Column,
      Column_row.min_size children Column,
      Column_row.min_update children Column )

let top ?(collapse_width = true) children =
  Widget
    ( Column_row.directional_draw Column_row.calc_when_start collapse_width
        children Column,
      Column_row.max_size collapse_width children Column,
      Column_row.directional_update Column_row.calc_when_start collapse_width
        children Column )

(* Functions for drawing row aligned to center. *)
let center ?(collapse_width = true) children =
  Widget
    ( Column_row.directional_draw
        (Column_row.calc_when_center Column)
        collapse_width children Column,
      Column_row.max_size collapse_width children Column,
      Column_row.directional_update
        (Column_row.calc_when_center Column)
        collapse_width children Column )

(* Functions for drawing row aligned to right. *)
let bottom ?(collapse_width = true) children =
  Widget
    ( Column_row.directional_draw
        (Column_row.calc_when_end Column)
        collapse_width children Column,
      Column_row.max_size collapse_width children Column,
      Column_row.directional_update
        (Column_row.calc_when_end Column)
        collapse_width children Column )

(* Functions for drawing row with space in between/around. *)
let space_between ?(collapse_width = true) children =
  Widget
    ( Column_row.draw_space_between collapse_width children Column,
      Column_row.max_size collapse_width children Column,
      Column_row.update_space_between collapse_width children Column )

let space_around ?(collapse_width = true) children =
  Widget
    ( Column_row.draw_space_around collapse_width children Column,
      Column_row.max_size collapse_width children Column,
      Column_row.update_space_around collapse_width children Column )
