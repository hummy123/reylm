open Reyml

let view num =
  Column.min
    [|
      Wrap.row_right ~row_padding:50
        [|
          Rect.widget ~width:num ~height:80 ~color:Raylib.Color.gray Empty;
          Conditional.exec (fun model -> model > 0) (fun model -> model + 10);
          Rect.widget ~width:num ~height:80 ~color:Raylib.Color.red Empty;
          Rect.widget ~width:num ~height:80 ~color:Raylib.Color.gray Empty;
          Rect.widget ~width:num ~height:80 ~color:Raylib.Color.red Empty;
          Rect.widget ~width:num ~height:80 ~color:Raylib.Color.gray Empty;
          Rect.widget ~width:num ~height:80 ~color:Raylib.Color.red Empty;
        |];
      Rect.widget ~width:num ~height:100 ~color:Raylib.Color.black Empty;
      Rect.widget ~width:num ~height:100 ~color:Raylib.Color.gold Empty;
      Rect.widget ~width:num ~height:100 ~color:Raylib.Color.skyblue Empty;
    |]

let num = 30
let () = run_app view num
