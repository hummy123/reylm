(* Load font once and then retrieve from hashtable. *)
let tbl = Hashtbl.create 1
let font_key = "fluent_font"

let get_font () =
  match Hashtbl.find_opt tbl font_key with
  | Some x -> x
  | None ->
      let dir =
        Raylib.get_application_directory ()
        ^ "resources/NotoSans/NotoSans-Regular.ttf"
      in
      let font = Raylib.load_font dir in
      Raylib.gen_texture_mipmaps (Raylib.addr (Raylib.Font.texture font));
      Raylib.set_texture_filter (Raylib.Font.texture font)
        Raylib.TextureFilter.Bilinear;
      Hashtbl.add tbl font_key font;
      font

let widget str ?(col = Raylib.Color.create 0 0 0 255) ?(font_size = 24.0)
    ?(font_spacing = 0.3) parent_x parent_y _ _
    (state_tree : State_tree.state_tree) model =
  let font = get_font () in
  let pos =
    Raylib.Vector2.create (float_of_int parent_x) (float_of_int parent_y)
  in
  Raylib.draw_text_ex font str pos font_size font_spacing col;
  let size = Raylib.measure_text_ex font str font_size font_spacing in
  let w = Raylib.Vector2.x size |> int_of_float in
  let h = Raylib.Vector2.y size |> int_of_float in
  (w, h, state_tree, model)

let size str ?(font_size = 24.0) ?(font_spacing = 3.0) _ _ _ =
  let font = get_font () in
  let size = Raylib.measure_text_ex font str font_size font_spacing in
  let w = Raylib.Vector2.x size |> int_of_float in
  let h = Raylib.Vector2.y size |> int_of_float in
  (w, h)
