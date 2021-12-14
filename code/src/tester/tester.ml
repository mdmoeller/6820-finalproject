open Core

let canonc st = List.stable_sort st ~compare:(Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare)

let to_str st = List.fold_left st ~init:"" ~f:(fun acc (i,j) -> Printf.sprintf "%s (%d,%d)" acc i j) |> String.strip

let gen_tests root = [
  (Kruskal.det_kruskal, "Deterministic Kruskal");
  (Kruskal.simp_rand_kruskal, "Simple random Kruskal");
  (Kruskal.prop_rand_kruskal, "Proportional random Kruskal");
  (Random_tree.with_root ~root:root, (Printf.sprintf "Random tree with root %d" root));
  (Random_tree.without_root, "Random tree without root");
]

let test ~graph ~iters ~batch =
  if (iters % batch) <> 0 then begin
    Printf.printf "ERROR: For test mode the number of iterations must be a multiple of the batch size. Exiting program.\n";
    ["ERROR"]
  end else 
    let sts = Kruskal.gen_all_sts ~graph in
    let reset () = Hashtbl.map_inplace sts ~f:(fun _ -> 0) in
    let test (f,typ) =
      let i = ref 0 in
      let maxt = ref Float.min_value in
      let mint = ref Float.max_value in
      let total = ref 0. in
      while !i < iters do
        let t1 = Unix.time () in
        for _ = 1 to batch do
          let res = f ~graph |> canonc |> to_str in
          Hashtbl.update sts res ~f:(function None -> 0 | Some x -> x + 1)
        done;
        let t2 = Unix.time () in
        let tm = t2 -. t1 in
        if Float.(tm < !mint) then mint := tm;
        if Float.(tm > !maxt) then maxt := tm;
        total := !total +. tm;
        i := !i + batch
      done;
      let avg = Float.(!total / ((of_int iters) / (of_int batch))) in
      let dist = Hashtbl.fold sts ~init:"" ~f:(fun ~key ~data acc -> Printf.sprintf "%s\t\t%s: %d\n" acc key data) in
      let res = Printf.sprintf "Test type: %s\n\tAverage: %f\n\tMax: %f\n\tMin: %f\n\tDistribution:\n%s\n"
        typ avg !maxt !mint dist
      in
      reset ();
      res
    in
    let root = Random.int (graph.v) in 
    let tests = gen_tests root in
    List.map tests ~f:test
