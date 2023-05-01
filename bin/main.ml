open Reyml

let view =
  Row.min
    [|
      Rect.widget ~width:100 ~height:50 (); Rect.widget ~width:50 ~height:100 ();
    |]

let () = run_app view
