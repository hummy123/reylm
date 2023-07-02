open Drawable

let min ?(collapse_width = true) children =
  Widget
    ( Column_row.min_draw collapse_width children Column,
      Column_row.min_size collapse_width children Column )

let top ?(collapse_width = true) children =
  Widget
    ( Column_row.directional_draw Column_row.calc_when_start collapse_width
        children Column,
      Column_row.max_size collapse_width children Column )

(* Functions for drawing row aligned to center. *)
let calc_start_y_center constraints flex_data =
  (constraints.max_height / 2) - (flex_data.occupied_non_flex_height / 2)

let center ?(collapse_width = true) children =
  Widget
    ( Column_row.directional_draw
        (Column_row.calc_when_center Column)
        collapse_width children Column,
      Column_row.max_size collapse_width children Column )

(* Functions for drawing row aligned to right. *)
let bottom ?(collapse_width = true) children =
  Widget
    ( Column_row.directional_draw
        (Column_row.calc_when_end Column)
        collapse_width children Column,
      Column_row.max_size collapse_width children Column )

(* Functions for drawing row with space in between/around. *)
let space_between ?(collapse_width = true) children =
  Widget
    ( Column_row.draw_space_between collapse_width children Column,
      Column_row.max_size collapse_width children Column )

let space_around ?(collapse_width = true) children =
  Widget
    ( Column_row.draw_space_around collapse_width children Column,
      Column_row.max_size collapse_width children Column )
