open Drawable_types

let button ?(text = "") ?width ?height ?(on_click = fun x -> x)
    ?(background_color = Raylib.Color.create 251 251 251 255)
    ?(text_color = Raylib.Color.create 32 28 28 255) name =
  (* Get minimum width and height if none provided, or if provided width and height are too small. *)
  let child_width, child_height =
    let width, height = Fluent_text.size text 0 0 0 in
    (width + 40, height + 10)
  in
  let width =
    match width with
    | Some x -> if x > child_width then x else child_width
    | None -> child_width
  in
  let height =
    match height with
    | Some x -> if x > child_height then x else child_height
    | None -> child_height
  in

  Other
    ( Fluent_button.widget name ~text ~width ~height ~on_click ~background_color
        ~text_color,
      Fluent_button.size width height,
      Empty )

let text ?(col = Raylib.Color.create 32 28 28 255) ?(font_size = 22.0)
    ?(font_spacing = 1.6) str =
  Other
    ( Fluent_text.widget str ~col ~font_size ~font_spacing,
      Fluent_text.size str ~font_size ~font_spacing,
      Empty )

let listview name =
  Other (Fluent_listview.widget name, Fluent_text.size "asdf", Empty)
