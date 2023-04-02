let widget parent_x parent_y parent_w parent_h state_tree model =
  let str =
    "wertyu
    [ok[pk[
      pl[pp[kp[k[p
      k[p
      k[pk[
        pk[
          pk[
            pk[
              k[
                k[pk[
                  pk[pki"
  in
  let img = Raylib.image_text str 20 Raylib.Color.raywhite in
  let texture = Raylib.load_texture_from_image img in
  Raylib.draw_texture texture parent_x parent_y Raylib.Color.red;
  (parent_w, parent_h, state_tree, model)
