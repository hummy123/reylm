open Reyml

let view =
  Row.start
    [|
      Flex.fill_width ~flex_val:5 (Rect.widget ~width:0 ~height:850 ());
      Rect.widget ~width:300 ~height:200 ~color:Raylib.Color.red ();
      Flex.fill_width ~flex_val:5
        (Rect.widget ~width:0 ~height:100 ~color:Raylib.Color.skyblue ());
    |]

let () = run_app view
