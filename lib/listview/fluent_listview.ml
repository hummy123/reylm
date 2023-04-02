let widget parent_x parent_y parent_w parent_h state_tree model =
  let str =
    "wertyuio\n\
    \  ohugyuhjklk\n\
    \  op;[]lopo\n\
    \  llp[k\n\
    \  l]\n\
    \  op]lpkl[]lpl[;lpll]lpl\n\
    \  k\n\
    \  [k\n\
    \  [pk[\n\
    \    pk[\n\
    \      kp\n\
    \      k\n\
    \      [pk[pk[\n\
    \        kp[\n\
    \          k\n\
    \          [pk[pk[pk[\n\
    \            kp[\n\
    \              pk\n\
    \              [pk[k\n\
    \              [k[\n\
    \                k[\n\
    \                  k\n\
    \                  [pk[kp[k[pk[\n\
    \                    k\n\
    \                    [k\n\
    \                    [k[\n\
    \                      k[\n\
    \                        k[k[\n\
    \                          k[\n\
    \                            k"
  in
  let img = Raylib.gen_image_text parent_w parent_h str in
  let texture = Raylib.load_texture_from_image img in
  Raylib.draw_texture texture parent_x parent_y Raylib.Color.red;
  (parent_w, parent_h, state_tree, model)
