open Graph_coloring

type 'a state = ('a Vertex.t * Color.t) list
  [@@deriving show, eq]

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

let () =
  let open Alcotest in
  run "Graph Coloring" [
    "Graph coloring", [
      test_case "Graph #1" `Quick test_graph_1;
      test_case "Graph #2" `Quick test_graph_2;
    ]
  ]
