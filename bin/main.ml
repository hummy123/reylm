open Reyml

(*
  Example 1.
 *)

(* These keys uniquely identify each Indeterminate_progress_bar so
   we can keep track of state (handled by framework) for each. *)
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
  This uses a Determinate_progress_bar which takes a percentage from the model.
 *)

open Reyml

type model = { progress : float }

let initial_model = { progress = 0.0 }
let update model = { progress = model.progress +. 0.001 }

let view model =
  Padding.all 100
    (Center.widget
       (Row.center
          [|
            (* Take progress from model. *)
            Determinate_progress_bar.horizontal ~percent:model.progress
              ~width:800 ();
            (* If the progress is less than 1.0, call the update function on the model. *)
            Conditional.exec (model.progress < 1.0) update;
          |]))

let () = run_app view initial_model
