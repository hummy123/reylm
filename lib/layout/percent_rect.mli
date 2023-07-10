open Drawable

val widget :
  ?radius:float ->
  ?color:Raylib.Color.t ->
  ?width:float ->
  ?height:float ->
  'a drawable ->
  'a drawable
