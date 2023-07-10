module Default_data = struct
  let default_background = Raylib.Color.create 214 214 214 255
  let default_foreground = Raylib.Color.create 0 102 180 255
  let radius = 1.0
  let direction_size = max_int
  let anti_direction_size = 5
end

module Default = Determinate_progress_bars.Make (Default_data)
