open Core

module Vertex = struct
  type 'a t = 'a
    [@@deriving show, eq]
end

module Edge = struct
  type 'a t = 'a Vertex.t * 'a Vertex.t
    [@@deriving show, eq]
end

module type COMPARABLE = sig
  type t
  val equal : t -> t -> bool
end

module type Graph = sig
  module type S = sig
    type elt

    type t

    val create : elt Vertex.t list * elt Edge.t list -> t

    val fold_vertices : t -> init:'a -> f:('a -> elt -> 'a) -> 'a

    val get_vertex_neighbors : t -> vertex:(elt Vertex.t) -> elt Vertex.t list

    val elt_eq : elt -> elt -> bool
  end

  module Make (C : COMPARABLE) : S with type elt = C.t
end

module Graph : Graph = struct
  module type S = sig
    type elt

    type t

    val create : elt Vertex.t list * elt Edge.t list -> t

    val fold_vertices : t -> init:'a -> f:('a -> elt -> 'a) -> 'a

    val get_vertex_neighbors : t -> vertex:(elt Vertex.t) -> elt Vertex.t list

    (* TODO: ugly workaround *)
    val elt_eq : elt -> elt -> bool
  end

  module Make (C : COMPARABLE) = struct
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
end

module Color = struct
  type t = int
    [@@deriving show, eq]
end

type 'a state = ('a Vertex.t * Color.t) list
  [@@deriving show, eq]

module type Coloring = sig
  module Make (G : Graph.S) : sig
    val color : G.t -> (G.elt Vertex.t * Color.t) list
  end
end

module Coloring : Coloring = struct
  module Make (G : Graph.S) = struct
    let rec color graph =
      G.fold_vertices graph ~init:[] ~f:(fun colored_vertices vertex ->
          let selected_color = select_color colored_vertices graph vertex in
          (vertex, selected_color) :: colored_vertices
      )
      |> List.rev

    and select_color colored_vertices (graph : G.t) vertex =
      let neighbor_colors =
      G.get_vertex_neighbors graph ~vertex:vertex
      |> List.filter_map ~f:(get_vertex_color colored_vertices)
      in
      select_color' neighbor_colors 1

    and select_color' neighbor_colors color =
      if List.mem neighbor_colors color ~equal:(Color.equal)
      then select_color' neighbor_colors (color + 1)
      else color

    and get_vertex_color colored_vertices vertex =
      colored_vertices
      |> List.find ~f:(fun v -> G.elt_eq vertex (fst v))
      |> Option.map ~f:snd
  end
end

module Tests = struct
  let state_testable e = Alcotest.testable (pp_state (Alcotest.pp e)) (equal_state (Alcotest.equal e))

  module IntGraph = Graph.Make(Int)
  module IntColoring = Coloring.Make(IntGraph)
  
  let test_graph_1 () =
    let g = IntGraph.create (
      [1; 2; 3; 4; 5; 6; 7; 8],
      [
        (1, 2); (1, 3); (1, 4); (1, 7);
        (2, 1); (2, 5); (2, 6);
        (3, 1); (3, 7);
        (4, 1); (4, 7);
        (5, 2); (5, 6); (5, 8);
        (6, 2); (6, 5); (6, 8);
        (7, 1); (7, 3); (7, 4);
        (8, 5); (8, 6); (8, 7);
      ]
    ) in
    Alcotest.(check (state_testable int))
      "valid coloring" 
      [(1, 1); (2, 2); (3, 2); (4, 2); (5, 1); (6, 3); (7, 3); (8, 2)] 
      (IntColoring.color g)
  
  let test_graph_2 () =
    let g = IntGraph.create (
      [1; 2; 3; 4; 5; 6;],
      [
        (1, 2); (1, 4); (1, 6);
        (2, 1); (2, 3); (2, 5);
        (3, 2); (3, 4); (3, 6);
        (4, 1); (4, 3); (4, 5);
        (5, 2); (5, 4); (5, 6);
        (6, 1); (6, 3); (6, 5);
      ]
    ) in
    Alcotest.(check (state_testable int))
      "valid coloring" 
      [(1, 1); (2, 2); (3, 1); (4, 2); (5, 1); (6, 2)]
      (IntColoring.color g)
end

let () =
  let open Alcotest in
  run "Graph Coloring" [
    "Graph coloring", [
      test_case "Graph #1" `Quick Tests.test_graph_1;
      test_case "Graph #2" `Quick Tests.test_graph_2;
    ]
  ]
