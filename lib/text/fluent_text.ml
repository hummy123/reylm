open Fonts

let widget str ?(font_path = Fonts.regular)
    ?(col = Raylib.Color.create 32 28 28 255) ?(font_size = 22.0)
    ?(font_spacing = 1.6) parent_x parent_y _ _
    (state_tree : State_tree.state_tree) model =
  let font = get_font font_path font_size in
  let pos =
    Raylib.Vector2.create (float_of_int parent_x) (float_of_int parent_y)
  in
  Raylib.draw_text_ex font str pos font_size font_spacing col;
  let size = Raylib.measure_text_ex font str font_size font_spacing in
  let w = Raylib.Vector2.x size |> int_of_float in
  let h = Raylib.Vector2.y size |> int_of_float in
  (w, h, state_tree, model)

let size str ?(font_path = Fonts.regular) ?(font_size = 22.0)
    ?(font_spacing = 1.6) _ _ _ =
  let font = get_font font_path font_size in
  let size = Raylib.measure_text_ex font str font_size font_spacing in
  let w = Raylib.Vector2.x size |> int_of_float in
  let h = Raylib.Vector2.y size |> int_of_float in
  (w, h)
