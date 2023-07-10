open Constraints
open Drawable

let size width height constraints =
  let width =
    float_of_int constraints.max_width *. width |> Float.round |> int_of_float
  in
  let height =
    float_of_int constraints.max_height *. height |> Float.round |> int_of_float
  in
  { width; height }

let draw width height child constraints =
  (* Draw. *)
  let f_width = float_of_int constraints.max_width *. width |> Float.round in
  let f_height = float_of_int constraints.max_height *. height |> Float.round in

  (* Set child constraints and, in last line, return. *)
  let width = int_of_float f_width in
  let height = int_of_float f_height in
  let child_constraints =
    { constraints with max_height = height; max_width = width }
  in
  let _ = draw child_constraints child in
  { width; height }

let update width height child constraints model =
  let width =
    float_of_int constraints.max_width *. width |> Float.round |> int_of_float
  in
  let height =
    float_of_int constraints.max_height *. height |> Float.round |> int_of_float
  in
  let child_constraints =
    { constraints with max_height = height; max_width = width }
  in
  let { model; _ } = Drawable.update child_constraints model child in
  { width; height; model }

let widget ?(width = 1.0) ?(height = 1.0) child =
  let width = if width < 0.0 then 0.0 else if width > 1.0 then 1.0 else width in
  let height =
    if height < 0.0 then 0.0 else if height > 1.0 then 1.0 else height
  in
  Widget (draw width height child, size width height, update width height child)
