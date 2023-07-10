open Drawable

val widget :
  ?radius:float ->
  ?color:Raylib.Color.t ->
  width:int ->
  height:int ->
  'a drawable ->
  'a drawable
(** 
    Same as {!Sized_box.widget} but optionally takes a radius and a colour, making it visible. 
    This can be helpful for debugging. Give the widget a random colour and you can see how much space it takes.
    *)
