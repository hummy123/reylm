open Reyml

let view =
  Center.widget
    (Sized_box.widget ~width:300 ~height:700
       (Column.space_around
          [|
            (if int_of_float (Sys.time ()) mod 2 = 0 then
               Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray
                 (Center.widget
                    (Row.space_around ~expand_height:false
                       [|
                         Flex.fill_width
                           (Rect.widget ~width:50 ~height:50
                              ~color:Raylib.Color.orange Empty);
                         Flex.fill_width
                           (Rect.widget ~width:50 ~height:50
                              ~color:Raylib.Color.skyblue Empty);
                         Flex.fill_height
                           (Rect.widget ~width:50 ~height:50
                              ~color:Raylib.Color.yellow Empty);
                       |]))
             else Empty);
            Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
            Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.black
              (Center.widget
                 (Row.space_around ~expand_height:true
                    [|
                      Rect.widget ~width:50 ~height:50 ~color:Raylib.Color.white
                        Empty;
                      Rect.widget ~width:50 ~height:50 ~color:Raylib.Color.white
                        Empty;
                      Rect.widget ~width:50 ~height:50 ~color:Raylib.Color.white
                        Empty;
                    |]));
            Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
            Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray Empty;
          |]))

let () = run_app view
