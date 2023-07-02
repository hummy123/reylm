open Reyml

let view =
  Center.widget
    (Sized_box.widget ~width:300 ~height:700
       (Column.space_around
          [|
            Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray
              (Center.widget
                 (Row.space_around ~collapse_height:false
                    [|
                      Rect.widget ~width:50 ~height:50
                        ~color:Raylib.Color.orange Empty;
                      Rect.widget ~width:50 ~height:50
                        ~color:Raylib.Color.skyblue Empty;
                      Flex.fill_height
                        (Rect.widget ~width:50 ~height:50
                           ~color:Raylib.Color.gold Empty);
                    |]));
            Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red Empty;
            Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.black
              (Center.widget
                 (Row.space_around ~collapse_height:true
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
