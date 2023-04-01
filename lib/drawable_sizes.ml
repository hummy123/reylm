open Drawable_types

let rec size parent_w parent_h = function
  | Empty -> (0, 0)
  | HLine _ -> (parent_w, 1)
  | Border (_, _, _, d) -> size parent_w parent_h d
  | Rect (w, h, _, _, _) ->
      let w = if w < parent_w then w else parent_w in
      let h = if h < parent_h then h else parent_h in
      (w, h)
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
  | Padding (l, t, r, b, d) -> size parent_w parent_h d
  | Other (_, f_calc, d) -> f_calc parent_w parent_h d
