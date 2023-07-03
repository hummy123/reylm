open Reyml

let view =
  Column.min
    [|
      Row.min
        [|
          Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray Empty;
          Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
        |];
      Row.min
        [|
          Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
          Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray Empty;
          Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
        |];
    |]

let () = run_app view
