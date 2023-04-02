open Reyml
open Reyml.Drawable_types

type model = { counter : int }

let initial_model = { counter = 0 }

let placeholder model =
  Overlay
    [
      RowCenter
        [
          ColumnCenter
            [
              Rect
                ( 100,
                  100,
                  0.0,
                  Raylib.Color.blank,
                  ColumnCenter
                    [
                      Padding
                        ( 10,
                          10,
                          10,
                          10,
                          Fluent.text
                            (Format.sprintf "Clicked %i times!" model.counter)
                        );
                      RowCenter
                        [
                          Fluent.button "Counter_button" ~text:"Click me"
                            ~on_click:(fun model ->
                              { counter = model.counter + 1 });
                        ];
                    ] );
            ];
        ];
    ]

let () = Reyml.run_app "Test" placeholder initial_model
