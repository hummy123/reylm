open Drawable

let size flex_val fit child constraints =
  let flex = Flex (flex_val, fit, child) in
  Drawable.size constraints flex

let draw flex_val fit child constraints =
  let flex = Flex (flex_val, fit, child) in
  Drawable.draw constraints flex

let expand ?(flex_val = 1) child =
  Widget (draw flex_val Expand child, size flex_val Expand child)

let fill_height ?(flex_val = 1) child =
  Widget (draw flex_val FillHeight child, size flex_val FillHeight child)

let fill_width ?(flex_val = 1) child =
  Widget (draw flex_val FillWidth child, size flex_val FillWidth child)

let natural_size ?(flex_val = 1) child =
  Widget (draw flex_val NaturalSize child, size flex_val NaturalSize child)
