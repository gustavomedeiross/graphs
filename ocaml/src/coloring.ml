open Core

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
