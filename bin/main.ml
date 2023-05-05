open Reyml

let view =
  Row.min
    [|
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray ();
      Flex.fill_width
        (Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red ());
      Flex.fill_width
        (Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.black ());
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red ();
      Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray ();
    |]

let () = run_app view
