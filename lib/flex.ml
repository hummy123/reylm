open Drawable

let size flex_val fit child constraints =
  let flex = Flex (flex_val, fit, child) in
  Drawable.size constraints flex

let draw flex_val fit child constraints =
  let flex = Flex (flex_val, fit, child) in
  Drawable.draw constraints flex

let widget ?(flex_val = 1) ?(fit = ForceFill) child =
  Widget (draw flex_val fit child, size flex_val fit child)
