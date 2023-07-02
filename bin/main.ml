open Reyml

let view =
  Center.widget
    (Column.space_around
       [|
         Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray
           (Center.widget
              (Row.space_around ~collapse_height:false
                 [|
                   Rect.widget ~width:50 ~height:50 ~color:Raylib.Color.orange
                     Empty;
                   Rect.widget ~width:50 ~height:50 ~color:Raylib.Color.skyblue
                     Empty;
                   Flex.fill_height
                     (Rect.widget ~width:50 ~height:50 ~color:Raylib.Color.gold
                        Empty);
                 |]));
         Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red
           (Column.space_around ~collapse_width:false
              [|
                Flex.fill_width
                  (Rect.widget ~width:10 ~height:10
                     ~color:Raylib.Color.darkpurple Empty);
                Flex.fill_width
                  (Rect.widget ~width:10 ~height:10
                     ~color:Raylib.Color.darkgreen Empty);
                Flex.fill_width
                  (Rect.widget ~width:10 ~height:10
                     ~color:Raylib.Color.darkbrown Empty);
              |]);
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
         Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.red
           (Column.space_around ~collapse_width:false
              [|
                Flex.fill_width
                  (Rect.widget ~width:10 ~height:10
                     ~color:Raylib.Color.darkbrown Empty);
                Flex.fill_width
                  (Rect.widget ~width:10 ~height:10
                     ~color:Raylib.Color.darkgreen Empty);
                Flex.fill_width
                  (Rect.widget ~width:10 ~height:10
                     ~color:Raylib.Color.darkpurple Empty);
              |]);
         Rect.widget ~width:300 ~height:80 ~color:Raylib.Color.gray
           (Center.widget
              (Row.space_around ~collapse_height:false
                 [|
                   Flex.fill_height
                     (Rect.widget ~width:50 ~height:50 ~color:Raylib.Color.gold
                        Empty);
                   Column.bottom
                     [|
                       Rect.widget ~width:50 ~height:50
                         ~color:Raylib.Color.skyblue Empty;
                     |];
                   Column.bottom
                     [|
                       Rect.widget ~width:50 ~height:50
                         ~color:Raylib.Color.orange Empty;
                     |];
                 |]));
       |])

let () = run_app view
