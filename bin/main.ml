open Reyml

let view =
  Row.start
    [|
      Flex.fill_width ~flex_val:5 (Rect.widget ~width:80 ~height:850 ());
      Rect.widget ~width:300 ~height:200 ~color:Raylib.Color.red ();
      Rect.widget ~width:300 ~height:200 ~color:Raylib.Color.blue ();
      Rect.widget ~width:300 ~height:200 ~color:Raylib.Color.yellow ();
      Flex.fill_width ~flex_val:5
        (Rect.widget ~width:90 ~height:100 ~color:Raylib.Color.skyblue ());
    |]

let () = run_app view
