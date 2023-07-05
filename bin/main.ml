open Reyml

let view =
  Wrap.column_space_around
    [|
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray Empty;
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray Empty;
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray Empty;
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
    |]

let () = run_app view
