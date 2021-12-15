open Core

let canonc st = List.stable_sort st ~compare:(Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare)

let to_str st = List.fold_left st ~init:"" ~f:(fun acc (i,j) -> Printf.sprintf "%s (%d,%d)" acc i j) |> String.strip

let gen_tests root = [
  (Kruskal.det_kruskal, "Deterministic_Kruskal");
  (Kruskal.simp_rand_kruskal, "Simple_random_Kruskal");
  (Kruskal.prop_rand_kruskal, "Proportional_random_Kruskal");
  (Random_tree.without_root, "Random_tree_without_root");
  (Random_tree.with_root ~root:root, (Printf.sprintf "Random_tree_with_root_%d" root));
]

let test ~fname ~graph ~iters =
  let sts = Kruskal.gen_all_sts ~graph in
  let reset () = Hashtbl.map_inplace sts ~f:(fun _ -> 0) in
  let quota = Core_bench.Bench.Quota.Num_calls (iters) in 
  let config = Core_bench.Bench.Run_config.create ~quota:quota () in
  let analysis = [Core_bench.Bench.Analysis_config.nanos_vs_runs; Core_bench.Bench.Analysis_config.cycles_vs_runs] in 
  let test (f,typ) =
    let bench = Core_bench.Bench.Test.create ~name:typ (fun () ->
      let res = f ~graph |> canonc |> to_str in
      Hashtbl.update sts res ~f:(function None -> 0 | Some x -> x + 1))
    in
    Core_bench.Bench.bench
      ~run_config:config
      ~analysis_configs:analysis
      ~save_to_file:(fun _ -> Printf.sprintf "%s_%s_%s" fname typ "stats")
      [bench];
    let dist = Hashtbl.fold sts ~init:"" ~f:(fun ~key ~data acc -> Printf.sprintf "%s\t\t%s: %d\n" acc key data) in
    reset ();
    Printf.sprintf "Test type: %s\n\tDistribution:\n%s\n" typ dist
  in
  let root = Random.int (graph.v) in 
  let tests = gen_tests root in
  List.map tests ~f:test
