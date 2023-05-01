open Reyml

let view =
  Row.min ~collapse_height:false
    [|
      Flex.fill_width ~flex_val:5 (Rect.widget ~width:80 ~height:850 ());
      Rect.widget ~width:300 ~height:200 ~color:Raylib.Color.red ();
      Flex.fill_height ~flex_val:1 (Rect.widget ~width:80 ~height:5 ());
      Rect.widget ~width:300 ~height:200 ~color:Raylib.Color.yellow ();
      Flex.fill_width ~flex_val:5
        (Rect.widget ~width:90 ~height:100 ~color:Raylib.Color.skyblue ());
    |]

let () = run_app view
