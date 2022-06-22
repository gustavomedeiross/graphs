open Core

module type S = sig
  type elt

  type t

  val create : elt Vertex.t list * elt Edge.t list -> t

  val fold_vertices : t -> init:'a -> f:('a -> elt -> 'a) -> 'a

  val get_vertex_neighbors : t -> vertex:(elt Vertex.t) -> elt Vertex.t list

  (* TODO: ugly workaround *)
  val elt_eq : elt -> elt -> bool
end

module Make (C : Sig.COMPARABLE) = struct
  type elt = C.t

  type t = elt Vertex.t list * elt Edge.t list

  type adjacency = (elt * elt list) list

  let create = ident

  let graph_to_adjacency (vertices, edges : t) : adjacency =
      let adjacent_vertices vertex edges =
      edges
      |> List.filter ~f:(fun (x, y) -> (C.equal vertex x) || (C.equal vertex y))
      |> List.concat_map ~f:(function
          | (x, y) when C.equal vertex x -> [y]
          | (x, y) when C.equal vertex y -> [x]
          | _ -> [])
      in
      List.map vertices ~f:(fun v -> (v, (adjacent_vertices v edges)))

  let get_vertices graph : elt Vertex.t list = fst graph

  let fold_vertices graph ~init ~f =
      List.fold (get_vertices graph) ~init:init ~f:f

  let get_vertex_neighbors graph ~vertex =
      let adj = graph_to_adjacency graph in
      List.find adj ~f:(fun (v, _edges) -> C.equal vertex v)
      |> Option.map ~f:(fun (_, edges) -> edges)
      |> Option.value_exn

  let elt_eq = C.equal
end
