open Reyml

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
