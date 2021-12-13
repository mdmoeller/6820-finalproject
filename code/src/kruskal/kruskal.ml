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

let simple_rand edges = List.permute edges

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
  preprocess graph.e |> simple_rand |> kruskal graph.v

let prop_rand_kruskal ~graph:_ = failwith "Not yet implemented."
