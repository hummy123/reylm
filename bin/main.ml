open Reyml
open Reyml.Drawable

type model = { counter : int }

let initial_model = { counter = 0 }

let placeholder _ =
  RowStart
    [
      Padding
        ( 100,
          100,
          100,
          100,
          Other
            ( Fluent.button "asdf",
              Fluent.button_size,
              ColumnSpaceAround
                [
                  Other
                    ( Fluent.text "Standard button",
                      Fluent.text_size "Standard button",
                      Empty );
                ] ) );
      Padding
        ( 100,
          0,
          100,
          0,
          ColumnEnd [ Rect (100, 100, 0.3, Raylib.Color.red, Empty) ] );
      Padding
        ( 100,
          0,
          100,
          0,
          ColumnEnd
            [
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
            ] );
      Padding
        ( 100,
          0,
          20,
          0,
          ColumnEnd
            [
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
            ] );
      Padding
        ( 100,
          0,
          20,
          0,
          ColumnEnd
            [
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
            ] );
      Padding
        ( 100,
          0,
          20,
          0,
          ColumnEnd
            [
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
            ] );
    ]

let () = Reyml.run_app "Test" placeholder initial_model
