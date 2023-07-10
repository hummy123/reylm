open Drawable

val widget :
  ?radius:float ->
  ?color:Raylib.Color.t ->
  ?width:float ->
  ?height:float ->
  'a drawable ->
  'a drawable
(** 
    Like {!Percent_box.widget} but can take a colour and radius as well, making it visible.
    This can be helpful for debugging. Give the widget a random colour and you can see how much space it takes.
    *)
