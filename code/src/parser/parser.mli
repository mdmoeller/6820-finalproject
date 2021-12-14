type graph = {
  v : Int.t;
  e : ((Core.Int.t * Core.Float.t) Core.List.t) Core.Array.t;
}

val parse : Core.String.t -> graph
