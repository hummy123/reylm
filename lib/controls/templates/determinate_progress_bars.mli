(** Input signature of the functor {!Make}. *)
module type Determinate_progress_data = sig
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
  val horizontal :
    percent:float ->
    ?width:int ->
    ?height:int ->
    ?radius:float ->
    ?foreground:Raylib.Color.t ->
    ?background:Raylib.Color.t ->
    unit ->
    'a Drawable.drawable
  (** A horizontal progress bar. *)

  val vertical :
    percent:float ->
    ?width:int ->
    ?height:int ->
    ?radius:float ->
    ?foreground:Raylib.Color.t ->
    ?background:Raylib.Color.t ->
    unit ->
    'a Drawable.drawable
  (** A vertical progress bar. *)
end

module Make (Data : Determinate_progress_data) : S
