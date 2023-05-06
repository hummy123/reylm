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
let spacer = Spacer.vertical ()
let double_spacer = Spacer.vertical ~flex_val:2. ()

(* let draw_space_between collapse_height children constraints = *)
(*   let if_not_flex _ constraints = *)
(*     (* We will insert a spacer in between each child. *) *)
(*     let children = *)
(*       Array.fold_right (fun el acc -> spacer :: el :: acc) children [] *)
(*     in *)
(*     (* Remove first spacer from list. *) *)
(*     let children = *)
(*       match children with _ :: tail -> tail |> Array.of_list | _ -> [||] *)
(*     in *)
(*     let flex_data = calc_flex_data children constraints in *)
(*     flex_draw Column flex_data children constraints *)
(*   in *)
(*   flex_draw_if_flex_children collapse_height children constraints if_not_flex *)

(* let space_between ?(collapse_height = true) children = *)
(*   Widget *)
(*     ( draw_space_between collapse_height children, *)
(*       Column_row.max_size collapse_height children Column ) *)

(* let draw_space_around collapse_height children constraints = *)
(*   let if_not_flex _ constraints = *)
(*     let children = *)
(*       Array.fold_right *)
(*         (fun el acc -> double_spacer :: el :: acc) *)
(*         children [ spacer ] *)
(*       |> Array.of_list *)
(*     in *)
(*     Array.unsafe_set children 0 spacer; *)
(*     let flex_data = calc_flex_data children constraints in *)
(*     flex_draw Column flex_data children constraints *)
(*   in *)
(*   flex_draw_if_flex_children collapse_height children constraints if_not_flex *)

(* let space_around ?(collapse_height = true) children = *)
(*   Widget *)
(*     ( draw_space_around collapse_height children, *)
(*       Column_row.max_size collapse_height children Column ) *)
