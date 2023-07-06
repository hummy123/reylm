open Reyml

let view _ =
  Column.min
    [|
      Wrap.row_right ~row_padding:50
        [|
          Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray Empty;
          Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
          Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray Empty;
          Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
          Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray Empty;
          Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
        |];
      Rect.widget ~width:100 ~height:100 ~color:Raylib.Color.black Empty;
      Rect.widget ~width:100 ~height:100 ~color:Raylib.Color.gold Empty;
      Rect.widget ~width:100 ~height:100 ~color:Raylib.Color.skyblue Empty;
    |]

let () = run_app view ()
