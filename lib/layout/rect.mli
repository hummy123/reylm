open Drawable

val widget :
  ?radius:float ->
  ?color:Raylib.Color.t ->
  width:int ->
  height:int ->
  'a drawable ->
  'a drawable
