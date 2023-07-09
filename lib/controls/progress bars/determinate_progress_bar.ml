let default_bg = Raylib.Color.create 214 214 214 255
let default_fg = Raylib.Color.create 0 102 180 255

let view_horizontal percent width height radius foreground_col background_col =
  Rect.widget ~radius ~width ~height ~color:background_col
    (Percent_rect.widget ~radius ~width:percent ~height:1.0
       ~color:foreground_col Empty)

let horizontal ~percent ?(width = max_int) ?(height = 5) ?(radius = 1.0)
    ?(foreground_col = default_fg) ?(background_col = default_bg) () =
  view_horizontal percent width height radius foreground_col background_col

let view_vertical percent width height radius foreground_col background_col =
  Rect.widget ~radius ~width ~height ~color:background_col
    (Align.widget ~y_shift:1.0
       (Percent_rect.widget ~radius ~width:1.0 ~height:percent
          ~color:foreground_col Empty))

let vertical ~percent ?(width = 5) ?(height = max_int) ?(radius = 1.0)
    ?(foreground_col = default_fg) ?(background_col = default_bg) () =
  view_vertical percent width height radius foreground_col background_col
