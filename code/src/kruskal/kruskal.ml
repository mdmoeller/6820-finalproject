open Core
open Parser

let preprocess edges =
  let tbl = Hashtbl.create (module Int) in
  Array.iteri edges ~f:(fun i ls ->
    List.iter ls ~f:(fun (j,p) ->
      if i < j then Hashtbl.update tbl i ~f:(function Some ls -> (j,p) :: ls | None -> [(j,p)])));
  let tbl_ls = Hashtbl.to_alist tbl in
  List.concat_map tbl_ls ~f:(fun (i,v) -> List.map v ~f:(fun (j,p) -> (i,j,p)))
  |> List.sort ~compare:(fun (_,_,p1) (_,_,p2) -> Float.compare p1 p2)

let kruskal v e =
  let f = ref [] in
  let u = Array.init v ~f:Union_find.create in
  List.iter e ~f:(fun (i,j,_) ->
    if Union_find.same_class u.(i) u.(j) |> not then begin 
      f := (i,j) :: (*(j,i) :: *) !f;
      Union_find.union u.(i) u.(j)
    end);
  !f 

let det_kruskal ~graph =
  preprocess graph.e |> kruskal graph.v

let simp_rand_kruskal ~graph =
  preprocess graph.e |> List.permute |> kruskal graph.v

let prop_rand_kruskal ~graph =
  let sorted_edges = preprocess graph.e in 
  let rec f acc curr =
    if List.is_empty curr then acc
    else 
      let sum = List.fold_left curr ~init:0. ~f:(fun acc (_,_,p) -> acc +. p) in
      let curr' = List.folding_map curr ~init:0. ~f:(fun acc (i,j,p) -> 
        let n = Float.((p / sum + acc)) in (n, (i, j, n)))
      in
      let rnd = Random.float 1. in
      let (i,j,p) = List.find_exn curr' ~f:(fun (_,_,p) -> Float.(rnd <= p)) in
      let curr'' = List.filter curr' ~f:(fun (i',j',p') -> i <> i' || j <> j' || Float.(p <> p')) in
      f ((i,j,p) :: acc) curr''
  in
  f [] sorted_edges
  |> List.rev
  |> kruskal graph.v

let gen_all_sts ~graph =
  let rec fact n acc = if n = 0 then acc else fact (n - 1) (acc * n) in 
  let canonc st = List.stable_sort st ~compare:(fun (i1,j1) (i2,j2) -> Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (i1,j1) (i2,j2)) in
  let to_str st = List.fold_left st ~init:"" ~f:(fun acc (i,j) -> Printf.sprintf "%s (%d,%d)" acc i j) |> String.strip in
  let weights = Hashtbl.create ~size:(Array.length graph.e * graph.v * 2) (module String) in
  Array.iteri graph.e ~f:(fun i l ->
    List.iter l ~f:(fun (j,p) -> 
      Hashtbl.update weights (Printf.sprintf "(%d,%d)" i j) ~f:(fun _ -> p)));
  let sorted_edges = preprocess graph.e in
  let perms = List.gen_permutations sorted_edges in
  let sorted_edges_len_fact = fact (List.length sorted_edges) 0 in
  let sts = Hashtbl.create ~size:sorted_edges_len_fact (module String) in
  let rdist = Hashtbl.create ~size:sorted_edges_len_fact (module String) in
  Quickcheck.iter perms ~f:(fun perm ->
    let res = kruskal graph.v perm |> canonc in
    let res_str = to_str res in 
    let weight = List.fold_left res ~init:1. ~f:(fun acc (i,j) -> acc *. (Hashtbl.find_exn weights (Printf.sprintf "(%d,%d)" i j))) in 
    Hashtbl.update rdist res_str ~f:(fun _ -> weight);
    Hashtbl.update sts res_str ~f:(fun _ -> 0));
    (*Hashtbl.update sts (kruskal graph.v perm |> canonc |> to_str) ~f:(fun _ -> 0)*)
  let total_weight = Hashtbl.fold rdist ~init:0. ~f:(fun ~key:_ ~data:p acc -> p +. acc) in 
  Hashtbl.map_inplace rdist ~f:(fun p -> Float.(p / total_weight));
  sts, rdist
