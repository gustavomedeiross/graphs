module Make (G : Graph.S) : sig
  val color : G.t -> (G.elt Vertex.t * Color.t) list
end
