open Reyml
open Reyml.Drawable

type model = { counter : int }

let initial_model = { counter = 0 }

let placeholder _ =
  RowStart
    [
      Padding
        ( 100,
          0,
          100,
          0,
          ColumnSpaceAround [ Rect (100, 100, 0.3, Raylib.Color.red, Empty) ] );
      Padding
        ( 100,
          0,
          100,
          0,
          ColumnSpaceAround
            [
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
              Rect (100, 100, 0.3, Raylib.Color.red, Empty);
            ] );
      Padding
        ( 100,
          0,
          20,
          0,
          ColumnSpaceAround
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
          ColumnSpaceAround
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
          ColumnSpaceAround
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

let () = Reyml.run_app placeholder initial_model
