open Reyml

(*
  Example 1.
 *)

let key = Indeterminate_progress_bar.key ()
let key2 = Indeterminate_progress_bar.key ()
let key3 = Indeterminate_progress_bar.key ()
let key4 = Indeterminate_progress_bar.key ()
let key5 = Indeterminate_progress_bar.key ()

let view model =
  Padding.all 100
    (Column.space_around
       [|
         Indeterminate_progress_bar.horizontal ~width:800 key;
         Indeterminate_progress_bar.horizontal ~width:400 key2;
         Indeterminate_progress_bar.horizontal ~width:200 key3;
         Indeterminate_progress_bar.horizontal ~width:400 key4;
         Indeterminate_progress_bar.horizontal ~width:800 key5;
       |])

let () = run_app view ()

(*
  Example 2.
  (Comment out the above code, and uncomment the code below.)
 *)

open Reyml

type model = { color : Raylib.Color.t }

let initial_model = { color = Raylib.Color.create 0 0 0 0 }

let update model =
  (* Get "red" from the Color.t in the model *)
  let rgb = Raylib.Color.r model.color in
  let rgb = rgb + 1 in
  let color = Raylib.Color.create rgb rgb rgb rgb in
  { color }

let view model =
  Center.widget
    (Row.center
       [|
         Rect.widget ~width:500 ~height:500 ~color:model.color Empty;
         Conditional.exec true update;
       |])

let () = run_app view initial_model
