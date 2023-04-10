open Drawable

let size flex_val fit child constraints =
  let flex = Flex (flex_val, fit, child) in
  Drawable.size constraints flex

let draw flex_val fit child constraints =
  let flex = Flex (flex_val, fit, child) in
  Drawable.draw constraints flex

let force_fill ?(flex_val = 1) child =
  Widget (draw flex_val ForceFill child, size flex_val ForceFill child)

let no_force ?(flex_val = 1) child =
  Widget (draw flex_val NoForce child, size flex_val NoForce child)
