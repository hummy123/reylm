open Drawable_types

let button ?(text = "") ?(width = 0) ?(height = 32) ?(on_click = fun x -> x)
    ?(background_color = Raylib.Color.create 251 251 251 255)
    ?(text_color = Raylib.Color.create 32 28 28 255) name =
  Other
    ( Fluent_button.widget name ~text ~width ~height ~on_click ~background_color
        ~text_color,
      Fluent_button.size,
      Empty )

let text = Fluent_text.widget
let text_size = Fluent_text.size
