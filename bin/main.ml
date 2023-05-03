open Reyml

let view =
  Column.bottom
    [|
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red ();
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red ();
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red ();
      Rect.widget ~width:300 ~height:50 ~color:Raylib.Color.yellow ();
      Rect.widget ~width:90 ~height:10 ~color:Raylib.Color.black ();
    |]

let () = run_app view
