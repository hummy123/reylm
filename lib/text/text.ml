open Constraints
open Drawable
open Raylib

(* Gets the number of bytes in a char from a UTF-8 string. *)
let get_utf8_length chr =
  (* Int comparison is slightly faster. *)
  let chr = int_of_char chr in
  if chr <= 0x7f then 1
  else if chr <= 0xdf then 2
  else if chr <= 0xef then 3
  else if chr <= 0xf7 then 4
  else
    (* Invalid UTF-8 start, but we don't want to raise exception so give a unique return value for caller to handle. *)
    0

let get_substring text utf8_pos length = if length = 0 then "?" else "A"

let rec size_text_in_bounds text font_size utf8_pos line_start_utf8 acc_width
    acc_height max_width_found max_height_found constraints =
  if utf8_pos = String.length text then
    { width = max_width_found; height = max_height_found }
  else
    let chr = String.get text utf8_pos in
    let chr_length = get_utf8_length chr in
    let substring = get_substring text utf8_pos chr_length in
    let chr_size = measure_text substring (int_of_float font_size) in
    if chr_size + acc_width <= constraints.max_width then
      size_text_in_bounds text font_size (utf8_pos + chr_length) line_start_utf8
        (acc_width + chr_size) acc_height max_width_found max_height_found
        constraints
    else
      let line_str =
        String.sub text line_start_utf8 (utf8_pos - line_start_utf8)
      in
      let text_size =
        measure_text_ex (get_font_default ()) line_str font_size 0.0
      in
      let text_height = int_of_float (Vector2.y text_size) in
      let max_width_found =
        max max_width_found (int_of_float (Vector2.x text_size))
      in
      let max_height_found = max max_height_found text_height in
      if acc_height + text_height <= constraints.max_height then
        size_text_in_bounds text font_size (utf8_pos + chr_length) utf8_pos 0
          (acc_height + text_height) max_width_found max_height_found
          constraints
      else { width = max_width_found; height = max_height_found }

let size_text_wrapped text font_size constraints =
  size_text_in_bounds text font_size 0 0 0 0 0 0 constraints

let rec draw_text_in_bounds text font_size utf8_pos line_start_utf8 acc_width
    acc_height (color : Color.t) constraints =
  if utf8_pos = String.length text then (
    let line_str =
      String.sub text line_start_utf8 (String.length text - line_start_utf8)
    in
    draw_text line_str constraints.start_x
      (constraints.start_y + acc_height)
      (int_of_float font_size) color;
    size_text_wrapped text font_size constraints)
  else
    let chr = String.get text utf8_pos in
    let chr_length = get_utf8_length chr in
    let substring = get_substring text utf8_pos chr_length in
    let chr_size = measure_text substring (int_of_float font_size) in
    if chr_size + acc_width <= constraints.max_width then
      draw_text_in_bounds text font_size (utf8_pos + chr_length) line_start_utf8
        (acc_width + chr_size) acc_height color constraints
    else
      let line_str =
        String.sub text line_start_utf8 (utf8_pos - line_start_utf8)
      in
      let text_height =
        measure_text_ex (get_font_default ()) line_str font_size 0.0
      in
      let text_height = int_of_float (Vector2.y text_height) in
      if acc_height + text_height <= constraints.max_height then
        let _ =
          draw_text line_str constraints.start_x
            (constraints.start_y + acc_height)
            (int_of_float font_size) color
        in
        draw_text_in_bounds text font_size (utf8_pos + chr_length) utf8_pos 0
          (acc_height + text_height) color constraints
      else size_text_wrapped text font_size constraints

let draw_text_wrapped text font_size color constraints =
  draw_text_in_bounds text font_size 0 0 0 0 color constraints

let update text font_size constraints model =
  let ({ width; height } : drawable_size) =
    size_text_wrapped text font_size constraints
  in
  { width; height; model }

let widget ~text ~font_size ?(color = Color.black) () =
  Widget
    ( draw_text_wrapped text font_size color,
      size_text_wrapped text font_size,
      update text font_size )
