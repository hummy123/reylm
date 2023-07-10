val expand : ?flex_val:float -> unit -> 'a Drawable.drawable
(** Distributes some amount of vertical and horizontal space between widgets in a {!Row} or {!Column}., 
    If multiple spacers are used, the distribution can be controlled with the flex_val property
    which works just like the {!Flex} functions.*)

val vertical : ?flex_val:float -> unit -> 'a Drawable.drawable
(** Distributes some amount of vertical space between widgets in a {!Row} or {!Column}., 
    If multiple spacers are used, the distribution can be controlled with the flex_val property
    which works just like the {!Flex} functions.*)

val horizontal : ?flex_val:float -> unit -> 'a Drawable.drawable
(** Distributes some amount of horizontal space between widgets in a {!Row} or {!Column}., 
    If multiple spacers are used, the distribution can be controlled with the flex_val property
    which works just like the {!Flex} functions.*)
