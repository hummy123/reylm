open Reyml

type model = { percent : float }

let initial = { percent = 0.0 }
let animate model = { percent = model.percent +. 0.01 }

let view model =
  Center.widget (Determinate_progress_bar.vertical ~percent:1.0 ())

let () = run_app view initial
