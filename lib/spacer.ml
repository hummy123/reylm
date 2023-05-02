let expand ?(flex_val = 1.) () =
  Flex.expand ~flex_val (Rect.widget ~width:0 ~height:0 ())

let vertical ?(flex_val = 1.) () =
  Flex.fill_height ~flex_val (Rect.widget ~width:0 ~height:0 ())

let horizontal ?(flex_val = 1.) () =
  Flex.fill_width ~flex_val (Rect.widget ~width:0 ~height:0 ())
