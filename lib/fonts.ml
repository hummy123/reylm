(* Paths to fonts. *)
let thin = "resources/NotoSans/NotoSans-Thin.ttf"
let thin_italic = "resources/NotoSans/NotoSans-ThinItalic.ttf"
let extra_light = "resources/NotoSans/NotoSans-ExtraLight.ttf"
let extra_light_italic = "resources/NotoSans/NotoSans-ExtraLightItalic.ttf"
let light = "resources/NotoSans/NotoSans-Light.ttf"
let light_italic = "resources/NotoSans/NotoSans-LightItalic.ttf"
let regular = "resources/NotoSans/NotoSans-Regular.ttf"
let regular_italic = "resources/NotoSans/NotoSans-Italic.ttf"
let medium = "resources/NotoSans/NotoSans-Medium.ttf"
let medium_italic = "resources/NotoSans/NotoSans-MediumItalic.ttf"
let semibold = "resources/NotoSans/NotoSans-SemiBold.ttf"
let semibold_italic = "resources/NotoSans/NotoSans-SemiBoldItalic.ttf"
let bold = "resources/NotoSans/NotoSans-Bold.ttf"
let bold_italic = "resources/NotoSans/NotoSans-BoldItalic.ttf"
let extra_bold = "resources/NotoSans/NotoSans-ExtraBold.ttf"
let extra_bold_italic = "resources/NotoSans/NotoSans-ExtraBoldItalic.ttf"
let black = "resources/NotoSans/NotoSans-Black.ttf"
let black_italic = "resources/NotoSans/NotoSans-BlackItalic.ttf"

(* Hashtbl to make sure we only load font once. *)
let tbl = Hashtbl.create 20

let get_font path (font_size : float) =
  match Hashtbl.find_opt tbl (path, font_size) with
  | Some x -> x
  | None ->
      let dir = Raylib.get_application_directory () ^ path in
      let font = Raylib.load_font_ex dir (int_of_float font_size) None in
      Raylib.gen_texture_mipmaps (Raylib.addr (Raylib.Font.texture font));
      Raylib.set_texture_filter (Raylib.Font.texture font)
        Raylib.TextureFilter.Bilinear;
      Hashtbl.add tbl (path, font_size) font;
      font
