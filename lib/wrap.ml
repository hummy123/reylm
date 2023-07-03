open Drawable

let split_children children constraints =
  let rec get_run pos acc_els acc_width =
    if pos = Array.length children then
      let row = Row.min (List.rev acc_els |> Array.of_list) in
      (pos - 1, row)
    else
      let child = Array.unsafe_get children pos in
      let { width; _ } = size constraints child in
      if width + acc_width <= constraints.max_width then
        get_run (pos + 1) (child :: acc_els) (acc_width + width)
      else
        let row = Row.min (List.rev acc_els |> Array.of_list) in
        (pos - 1, row)
  in
  let rec collect_runs pos acc =
    if pos = Array.length children then
      Column.min (acc |> List.rev |> Array.of_list)
    else
      let pos, row = get_run pos [] 0 in
      collect_runs (pos + 1) (row :: acc)
  in
  collect_runs 0 []

let size_row children constraints =
  let children = split_children children constraints in
  size constraints children

let draw_row children constraints =
  let children = split_children children constraints in
  draw constraints children

let row children = Widget (draw_row children, size_row children)
