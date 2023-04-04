(* There is a lot of data sent as input to the draw_widget function,
   and we just define a type for it so it's extensible and easier to keep track of
   due to fields being named. *)
type 'a draw_widget_input = {
  parent_x : int; (* The x coordinate this drawable should start drawing at. *)
  parent_y : int; (* The y coordinate this drawable should start drawing at. *)
  parent_w : int;
      (* The total width of this drawable's parent. This drawable should not go beyond this width. *)
  parent_h : int;
      (* The total height of this drawable's parent. This drawable should not go beyond this height. *)
  state_tree : State_tree.state_tree;
      (* State tree, passing state of controls such as animations through main loop. *)
  model : 'a; (* The user's domain model. *)
}

(* The data returned by each drawable. *)
type 'a draw_widget_output = {
  width : int;
  height : int;
  state_tree : State_tree.state_tree;
  model : 'a;
}

type width = int
type height = int
type radius = float
type left = int
type top = int
type right = int
type bottom = int
type thickness = float
type colour = Raylib.Color.t

(* 'a is type of the domain model for the user's app.' *)
type 'a drawable =
  | Empty
  | HLine of Raylib.Color.t
  | VLine of Raylib.Color.t
  | HPanel of 'a drawable list
  | VPanel of 'a drawable list
  | ColumnStart of 'a drawable list
  | ColumnCenter of 'a drawable list
  | ColumnEnd of 'a drawable list
  | ColumnSpaceAround of 'a drawable list
  | ColumnSpaceBetween of 'a drawable list
  | RowStart of 'a drawable list
  | RowCenter of 'a drawable list
  | RowEnd of 'a drawable list
  | RowSpaceAround of 'a drawable list
  | RowSpaceBetween of 'a drawable list
  | Overlay of 'a drawable list
  | Rect of width * height * radius * colour * 'a drawable
  | Padding of left * top * right * bottom * 'a drawable
  | Border of radius * Raylib.Color.t * thickness * 'a drawable
  | Other of
      ('a draw_widget_input -> int * int * State_tree.state_tree * 'a)
      * (int -> int -> 'a drawable -> int * int)
      * 'a drawable
