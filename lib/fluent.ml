open Drawable_types

let button ?(text = "") ?(width = 0) ?(height = 32) ?(on_click = fun x -> x)
    ?(background_color = Raylib.Color.create 251 251 251 255)
    ?(text_color = Raylib.Color.create 32 28 28 255) name =
  Other
    ( Fluent_button.widget name ~text ~width ~height ~on_click ~background_color
        ~text_color,
      Fluent_button.size,
      Empty )

let text ?(col = Raylib.Color.create 32 28 28 255) ?(font_size = 22.0)
    ?(font_spacing = 1.6) str =
  Other
    ( Fluent_text.widget str ~col ~font_size ~font_spacing,
      Fluent_text.size str ~font_size ~font_spacing,
      Empty )
