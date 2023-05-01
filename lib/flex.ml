open Drawable

let expand ?(flex_val = 1) child = Flex (flex_val, Expand, child)
let fill_height ?(flex_val = 1) child = Flex (flex_val, FillHeight, child)
let fill_width ?(flex_val = 1) child = Flex (flex_val, FillWidth, child)
let natural_size ?(flex_val = 1) child = Flex (flex_val, NaturalSize, child)
