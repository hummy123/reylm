type width = int
type height = int
type radius = float
type left = int
type top = int
type right = int
type bottom = int
type thickness = float
type colour = Raylib.Color.t
type vert_align = Top | Middle | Bottom
type hor_align = Left | Middle | Right

(* 'a is type of the domain model for the user's app.' *)
type 'a drawable =
  | ColumnStart of 'a drawable list
  | ColumnCenter of 'a drawable list
  | ColumnEnd of 'a drawable list
  | ColumnSpaceAround of 'a drawable list
  | ColumnSpaceBetween of 'a drawable list
  | RowStart of 'a drawable list
  | Rect of width * height * radius * colour * 'a drawable
  | Padding of left * top * right * bottom * 'a drawable
  | Border of radius * Raylib.Color.t * thickness * 'a drawable
  | Empty
  | Other of
      (int ->
      int ->
      int ->
      int ->
      State_tree.state_tree ->
      'a ->
      int * int * State_tree.state_tree * 'a)
      * (int -> int -> 'a drawable -> int * int)
      * 'a drawable
