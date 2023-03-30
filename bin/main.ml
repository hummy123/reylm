open Reyml
open Reyml.Drawable_types

type model = { counter : int }

let initial_model = { counter = 0 }

let placeholder _ =
  ColumnSpaceBetween
    [
      RowStart
        [
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Padding
            ( 50,
              50,
              50,
              50,
              Other
                ( Fluent.button "123" ~text:"Standard button",
                  Fluent.button_size,
                  ColumnCenter
                    [
                      RowCenter
                        [
                          Other
                            ( Fluent.text "adsfg",
                              Fluent.text_size "adsfg",
                              Empty );
                        ];
                    ] ) );
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
        ];
      RowCenter
        [
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
        ];
      RowEnd
        [
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
          Rect (100, 100, 0.3, Raylib.Color.red, Empty);
        ];
    ]

let () = Reyml.run_app "Test" placeholder initial_model
