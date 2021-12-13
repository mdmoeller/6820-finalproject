type graph = {
  v : Int.t;
  e : ((Int.t * Float.t) List.t) Array.t;
}

val parse : String.t -> graph
