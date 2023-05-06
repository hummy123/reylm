open Reyml

let view =
  Column.bottom
    [|
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray ();
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red ();
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.black ();
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red ();
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray ();
    |]

let () = run_app view
