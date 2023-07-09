open Reyml

type model = { percent : float }

let initial = { percent = 0.0 }
let animate model = { percent = model.percent +. 0.01 }
let key1 = Indeterminate_progress_bar.key ()
let key2 = Indeterminate_progress_bar.key ()

let view model =
  Padding.all 100
    (Row.space_around
       [|
         Padding.all 100
           (Determinate_progress_bar.vertical ~percent:model.percent ());
         Conditional.exec (model.percent < 1.0) animate;
         Padding.all 100
           (Determinate_progress_bar.horizontal ~percent:model.percent ());
       |])

let () = run_app view initial
