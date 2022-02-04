type t
type el = Tile.t

val print : t -> unit
val solve : t -> t
val top_left : t -> int
val make : int list -> t
