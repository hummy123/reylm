open Drawable

(* Functions for drawing row at minimum size required by children,
   not expanding as with normal behaviour. *)
let min_size children constraints =
  let width, height =
    Array.fold_left
      (fun (acc_w, max_h) el ->
        let size = Drawable.size constraints el in
        (acc_w + size.width, max max_h size.height))
      (0, 0) children
  in
  { width; height }

let min_draw children constraints =
  let _, width, height =
    Array.fold_left
      (fun (start_x, acc_w, max_h) el ->
        let constraints = { constraints with start_x } in
        let size = Drawable.draw constraints el in
        (* Values for next iteration. *)
        let start_x = start_x + size.width in
        let acc_w = size.width + acc_w in
        let max_h = max max_h size.height in
        (start_x, acc_w, max_h))
      (constraints.start_x, 0, 0)
      children
  in
  { width; height }

let min children = Widget (min_draw children, min_size children)

(* Functions for drawing row at maximum size. *)
let max_size children constraints =
  let { height; _ } = min_size children constraints in
  { width = constraints.max_width; height }
