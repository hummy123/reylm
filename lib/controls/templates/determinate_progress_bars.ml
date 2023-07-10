module type Determinate_progress_data = sig
  val default_foreground : Raylib.Color.t
  val default_background : Raylib.Color.t
  val radius : float
  val direction_size : int
  val anti_direction_size : int
end

module type S = sig
  val horizontal :
    percent:float ->
    ?width:int ->
    ?height:int ->
    ?radius:float ->
    ?background:Raylib.Color.t ->
    ?foreground:Raylib.Color.t ->
    unit ->
    'a Drawable.drawable

  val vertical :
    percent:float ->
    ?width:int ->
    ?height:int ->
    ?radius:float ->
    ?background:Raylib.Color.t ->
    ?foreground:Raylib.Color.t ->
    unit ->
    'a Drawable.drawable
end

module Make (Key : Determinate_progress_data) = struct
  let view_horizontal percent width height radius foreground background =
    Rect.widget ~radius ~width ~height ~color:background
      (Percent_rect.widget ~radius ~width:percent ~height:1.0 ~color:foreground
         Empty)

  let view_vertical percent width height radius foreground background =
    Rect.widget ~radius ~width ~height ~color:background
      (Align.widget ~y_shift:1.0
         (Percent_rect.widget ~radius ~width:1.0 ~height:percent
            ~color:foreground Empty))

  let horizontal ~percent ?(width = max_int) ?(height = 5)
      ?(radius = Key.radius) ?(foreground = Key.default_foreground)
      ?(background = Key.default_background) () =
    view_horizontal percent width height radius foreground background

  let vertical ~percent ?(width = 5) ?(height = max_int) ?(radius = Key.radius)
      ?(foreground = Key.default_foreground)
      ?(background = Key.default_background) () =
    view_vertical percent width height radius foreground background
end
