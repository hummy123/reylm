open Reyml

let view =
  Column.min
    [|
      Flex.fill_height ~flex_val:5. (Rect.widget ~width:80 ~height:850 ());
      Rect.widget ~width:300 ~height:200 ~color:Raylib.Color.red ();
      Flex.fill_height ~flex_val:1. (Rect.widget ~width:80 ~height:5 ());
      Rect.widget ~width:300 ~height:200 ~color:Raylib.Color.yellow ();
      Flex.fill_height ~flex_val:5.
        (Rect.widget ~width:90 ~height:100 ~color:Raylib.Color.black ());
    |]

let () = run_app view
