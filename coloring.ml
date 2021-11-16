open Core

type 'a vertex = 'a
  [@@deriving show]

type 'a edge = 'a * 'a
  [@@deriving show]

type 'a graph = Graph of 'a vertex list * 'a edge list
  [@@deriving show]

type 'a adjacency = Adjacency of ('a * 'a list) list
  [@@deriving show]

type color = int
  [@@deriving show]

let adj_vertices vertex edges =
  List.filter edges ~f:(fun (x, y) -> (Poly.equal vertex x) || (Poly.equal y vertex))
  |> List.map ~f:(function 
      | (x, y) when Poly.equal vertex x -> [y]
      | (x, y) when Poly.equal vertex y -> [x]
      | _ -> [])
  |> List.concat
  |> Set.Poly.of_list
  |> Set.Poly.to_list

let graph_to_adjacency = function
  | Graph (vertices, edges) ->
    Adjacency (List.map vertices ~f:(fun v -> (v, adj_vertices v edges)))

(* TODO: Is this required? *)
let sort_by_degree adjacency =
  List.sort adjacency ~compare:(fun a b -> Int.descending (List.length (snd a)) (List.length (snd b)))

let neighbors adj vertex =
  List.find adj ~f:(fun (v, _edges) -> Poly.equal vertex v)
  |> Option.map ~f:(fun (_, edges) -> edges)
  |> Option.value_exn

let get_vertex_color state vertex =
  List.find state ~f:(fun (v, _) -> Poly.equal vertex v)
  |> Option.map ~f:(fun (_, color) -> color)
  |> Option.value_exn

type state = (int vertex * color) list
  [@@deriving show]

let rec select_color' state nghbs color =
  let nghbs_colors = List.map nghbs ~f:(fun v -> get_vertex_color state v) in
  if List.exists nghbs_colors ~f:(fun c -> color = c)
    then select_color' state nghbs (color + 1)
    else color

let select_color state adj vertex =
  let nghbs = neighbors adj (fst vertex) in
  select_color' state nghbs 1

let rec color (state : state) (adjacency_list : 'a adjacency) =
  let Adjacency(adj) = adjacency_list in
  let current_vertex = List.find state ~f:(fun (_v, color) -> color = -1) in
  Out_channel.print_endline (show_state state);
  match current_vertex with
  | None -> state
  | Some vertex ->
    let selected_color = select_color state adj vertex in
    let new_state = List.map state ~f:(fun (v, color) -> if Poly.equal (fst vertex) v
      then (v, selected_color)
      else (v, color))
    in
    color new_state adjacency_list

let coloring (graph : 'a graph) : ('a vertex * color) list =
  let Adjacency(adj) = graph_to_adjacency graph in
  (* let sorted = sort_by_degree adj in *)
  let initial_state = List.map adj ~f:(fun v -> (fst v, -1)) in
  color initial_state (Adjacency adj)

let () =
  let sample_graph = Graph(
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
  let result = coloring sample_graph in
  Out_channel.print_endline (show_state result)
