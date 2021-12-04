open Core

type 'a vertex = 'a
  [@@deriving show, eq]

type 'a edge = 'a * 'a
  [@@deriving show, eq]

type 'a graph = 'a vertex list * 'a edge list
  [@@deriving show, eq]

type 'a adjacency = ('a * 'a list) list
  [@@deriving show, eq]

type color = int
  [@@deriving show, eq]

type 'a state = ('a vertex * color) list
  [@@deriving show, eq]

let graph_to_adjacency (vertices, edges) =
  let adjacent_vertices vertex edges =
    edges
    |> List.filter ~f:(fun (x, y) -> (Poly.equal vertex x) || (Poly.equal vertex y))
    |> List.concat_map ~f:(function 
        | (x, y) when Poly.equal vertex x -> [y]
        | (x, y) when Poly.equal vertex y -> [x]
        | _ -> [])
  in
  List.map vertices ~f:(fun v -> (v, (adjacent_vertices v edges)))

let get_vertex_color colored_vertices vertex =
  colored_vertices
  |> List.find ~f:(fun v -> Poly.equal vertex (fst v))
  |> Option.map ~f:snd

let neighbors (adj : 'a adjacency)  vertex =
  List.find adj ~f:(fun (v, _edges) -> Poly.equal vertex v)
  |> Option.map ~f:(fun (_, edges) -> edges)
  |> Option.value_exn

let rec select_color' neighbor_colors color =
  if List.mem neighbor_colors color ~equal:(Poly.equal)
    then select_color' neighbor_colors (color + 1)
    else color

let select_color colored_vertices (adjacency : 'a adjacency) vertex =
  let neighbor_colors =
    neighbors adjacency vertex
    |> List.filter_map ~f:(get_vertex_color colored_vertices)
  in
  select_color' neighbor_colors 1

let rec color adjacent_list vertices colored_vertices =
  match vertices with
  | [] -> List.rev colored_vertices
  | vertex :: remaining ->
      let selected_color = select_color colored_vertices adjacent_list vertex in
      color adjacent_list remaining ((vertex, selected_color) :: colored_vertices)

let coloring graph =
  color (graph_to_adjacency graph) (fst graph) []

let state_testable e = Alcotest.testable (pp_state (Alcotest.pp e)) (equal_state (Alcotest.equal e))

let test_graph_1 () =
  let g = (
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
    (coloring g)

let test_graph_2 () =
  let g = (
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
    (coloring g)

let () =
  let open Alcotest in
  run "Graph Coloring" [
    "Graph coloring", [
      test_case "Graph #1" `Quick test_graph_1;
      test_case "Graph #2" `Quick test_graph_2;
    ]
  ]
