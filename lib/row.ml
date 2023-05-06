open Drawable

let min ?(collapse_height = true) children =
  Widget
    ( Column_row.min_draw collapse_height children Row,
      Column_row.min_size collapse_height children Row )

let left ?(collapse_height = true) children =
  Widget
    ( Column_row.directional_draw Column_row.calc_when_start collapse_height
        children Row,
      Column_row.max_size collapse_height children Row )

let center ?(collapse_height = true) children =
  Widget
    ( Column_row.directional_draw
        (Column_row.calc_when_center Row)
        collapse_height children Row,
      Column_row.max_size collapse_height children Row )

(* Functions for drawing row aligned to right. *)
let calc_start_x_right constraints flex_data =
  constraints.max_width - flex_data.occupied_non_flex_width

let right ?(collapse_height = true) children =
  Widget
    ( Column_row.directional_draw
        (Column_row.calc_when_end Row)
        collapse_height children Row,
      Column_row.max_size collapse_height children Row )

(* (* Functions for drawing row with space in between/around. *) *)
(* let spacer = Spacer.horizontal () *)
(* let double_spacer = Spacer.horizontal ~flex_val:2. () *)

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
(*     flex_draw Row flex_data children constraints *)
(*   in *)
(*   flex_draw_if_flex_children collapse_height children constraints if_not_flex *)

(* let space_between ?(collapse_height = true) children = *)
(*   Widget *)
(*     ( draw_space_between collapse_height children, *)
(*       max_size collapse_height children ) *)

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
(*     flex_draw Row flex_data children constraints *)
(*   in *)
(*   flex_draw_if_flex_children collapse_height children constraints if_not_flex *)

(* let space_around ?(collapse_height = true) children = *)
(*   Widget *)
(*     ( draw_space_around collapse_height children, *)
(*       max_size collapse_height children ) *)
