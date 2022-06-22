module type S = sig
  type elt

  type t

  val create : elt Vertex.t list * elt Edge.t list -> t

  val fold_vertices : t -> init:'a -> f:('a -> elt -> 'a) -> 'a

  val get_vertex_neighbors : t -> vertex:(elt Vertex.t) -> elt Vertex.t list

    val elt_eq : elt -> elt -> bool
end

module Make (C : Sig.COMPARABLE) : S with type elt = C.t
