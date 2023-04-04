open Drawable_types

let rec size parent_w parent_h = function
  | Empty -> (0, 0)
  | HLine _ -> (parent_w, 1)
  | VLine _ -> (1, parent_h)
  | Border (_, _, _, d) -> size parent_w parent_h d
  | Rect (w, h, _, _, _) ->
      let w = if w < parent_w then w else parent_w in
      let h = if h < parent_h then h else parent_h in
      (w, h)
  | HPanel lst ->
      List.fold_left
        (fun (acc_w, max_h) el ->
          let c_w, c_h = size parent_w parent_h el in
          let max_h = if c_h > max_h then c_h else max_h in
          let acc_w = acc_w + c_w in
          (acc_w, max_h))
        (0, 0) lst
  | VPanel lst ->
      List.fold_left
        (fun (max_w, acc_h) el ->
          let c_w, c_h = size parent_w parent_h el in
          let max_w = if c_w > max_w then c_w else max_w in
          let acc_h = acc_h + c_h in
          (max_w, acc_h))
        (0, 0) lst
  | ColumnStart lst
  | ColumnCenter lst
  | ColumnEnd lst
  | ColumnSpaceAround lst
  | ColumnSpaceBetween lst ->
      let max_w =
        List.fold_left
          (fun max_w el ->
            let c_w, _ = size parent_w parent_h el in
            let max_w = if c_w > max_w then c_w else max_w in
            max_w)
          0 lst
      in
      (max_w, parent_h)
  | RowStart lst
  | RowCenter lst
  | RowEnd lst
  | RowSpaceAround lst
  | RowSpaceBetween lst ->
      let max_h =
        List.fold_left
          (fun (max_h : int) el ->
            let _, c_h = size parent_w parent_h el in
            let max_h = if c_h > max_h then c_h else max_h in
            max_h)
          0 lst
      in
      (parent_w, max_h)
  | Padding (l, t, r, b, d) ->
      let c_w, c_h = size parent_w parent_h d in
      (c_w + l + r, c_h + t + b)
  | Overlay d ->
      List.fold_left
        (fun (max_w, max_h) el ->
          let w, h = size parent_w parent_h el in
          let w = if w > max_w then w else max_w in
          let h = if h > max_h then h else max_h in
          (w, h))
        (0, 0) d
  | Other (_, f_calc, d) -> f_calc parent_w parent_h d
