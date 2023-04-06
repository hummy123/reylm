(* There is a lot of data sent as input to the draw_widget function,
   and we just define a type for it so it's extensible and easier to keep track of
   due to fields being named. *)
type 'a draw_widget_input = {
  (* The x coordinate this drawable should start drawing at. *)
  parent_x : int;
  (* The y coordinate this drawable should start drawing at. *)
  parent_y : int;
  (* The total width of this drawable's parent. This drawable should not go beyond this width. *)
  parent_w : int;
  (* The total height of this drawable's parent. This drawable should not go beyond this height. *)
  parent_h : int;
  (* State tree, passing state of controls such as animations through main loop. *)
  state_tree : State_tree.state_tree;
  (* The user's domain model. *)
  model : 'a;
}

(* The data returned by each drawable. *)
type 'a draw_widget_output = {
  (* The width of the control that returns this record. *)
  width : int;
  (* The height of the control that returns this record. *)
  height : int;
  (* State tree, passing state of controls such as animations through main loop. *)
  state_tree : State_tree.state_tree;
  (* The user's domain model. *)
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
      ('a draw_widget_input -> 'a draw_widget_output)
      * (int -> int -> 'a drawable -> int * int)
      * 'a drawable
