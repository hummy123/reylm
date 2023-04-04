type listview_state = {
  start_x : int;
  start_y : int;
  friction : float;
  h_dir : float;
  v_dir : float;
  texture : Raylib.Texture.t option;
}

let initial =
  {
    start_x = 0;
    start_y = 0;
    friction = 0.0;
    h_dir = 0.0;
    v_dir = 0.0;
    texture = None;
  }
