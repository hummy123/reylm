module type Indeterminate_progress_data = sig
  type key
  (** The type representing the progress bar's unique key. *)

  val default_foreground : Raylib.Color.t
  (** The colour of the inner bar that moves. *)

  val default_background : Raylib.Color.t
  (** The colour of the outer bar in the background. *)

  val radius : float
  (** The radius of both the inner and outer bar. *)

  val direction_size : int
  (** The width of a horizontal progress bar or the height of a vertical one. *)

  val anti_direction_size : int
  (** The height of a horizontal progress bar or the width of a vertical one. *)
end

module type S = sig
  type key
  (** The type representing the progress bar's unique key. *)

  val did_change : unit -> bool
  (** A function describing whether the control's state has changed. 
      Do not call this yourself. 
      If you generated a control through a functor, 
      wrap this function in an array and pass it off to {!Reyml.register}. *)

  val horizontal :
    key:key ->
    ?width:int ->
    ?height:int ->
    ?radius:float ->
    ?background:Raylib.Color.t ->
    ?foreground:Raylib.Color.t ->
    unit ->
    'a Drawable.drawable
  (** A horizontal progress bar. *)

  val vertical :
    key:key ->
    ?width:int ->
    ?height:int ->
    ?radius:float ->
    ?background:Raylib.Color.t ->
    ?foreground:Raylib.Color.t ->
    unit ->
    'a Drawable.drawable
  (** A vertical progress bar. *)
end

module Make (Data : Indeterminate_progress_data) : S with type key = Data.key
