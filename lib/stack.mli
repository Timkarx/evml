type t

val empty : t
val push : int -> t -> t
val pop : t -> (int * t)
val repr : t -> unit
