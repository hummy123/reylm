open Reyml

type model = { percent : float }

let initial = { percent = 0.0 }
let animate model = { percent = model.percent +. 0.01 }

let key1 = Indeterminate_progress_bar.key()
let key2 = Indeterminate_progress_bar.key()
let view model =
  Column.space_around ~collapse_width:false
  [|
    Conditional.exec (fun _ -> true) animate;
    Indeterminate_progress_bar.vertical key1;
    Indeterminate_progress_bar.horizontal key2;
  |]

let () = run_app view initial
