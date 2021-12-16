open Core
open Parser

let normalize1 e = 
  Array.map e ~f:(fun out ->
    let sum = List.fold_left out ~init:0. ~f:(fun a (_,p) -> p +. a) in
    let norm = List.map out ~f:(fun (i,p) -> (i,p /. sum)) in
    List.folding_map norm ~init:0. ~f:(fun a (i,p) -> ((a +. p), (i, a +. p))))

let normalize2 e =
  let float_cmp (_,p1) (_,p2) = Float.compare p1 p2 in
  let val_map default opt = Option.value_map opt ~default:default ~f:snd in
  let max, maxes = Array.fold_map e ~init:0. ~f:(fun cur_max ls ->
    let ls_max = List.max_elt ls ~compare:float_cmp |> val_map 0. in
    if Float.(ls_max > cur_max) then (ls_max,ls_max) else (cur_max,ls_max))
  in
  let e = Array.mapi e ~f:(fun i curr ->
      if Float.equal (max -. maxes.(i)) 0. then curr else (i, max -. maxes.(i)) :: curr)
  in
  normalize1 e

let rand_succ out =
  let rnd = Random.float 1. in
  List.find_exn out ~f:(fun (_,p) -> Float.(rnd <= p)) |> fst

let _print e =
  Array.iteri e ~f:(fun i a -> Printf.printf "%d:" i; List.iter a ~f:(fun (j,p) -> Printf.printf "(%d,%f) " j p); Printf.printf "\n")

let with_root ~root ~graph =
  let e = normalize1 graph.e in
  let in_tree = Array.init graph.v ~f:(fun _ -> false) in
  let next = Array.init graph.v ~f:(fun _ -> -1) in
  in_tree.(root) <- true;
  let u = ref (-1) in 
  for i = 0 to (graph.v - 1) do
    u := i;
    while in_tree.(!u) |> not do
      next.(!u) <- rand_succ e.(!u);
      u := next.(!u)
    done;
    u := i;
    while in_tree.(!u) |> not do
      in_tree.(!u) <- true;
      u := next.(!u)
    done
  done;
  Array.filter_mapi next ~f:(fun i j ->
    if i = -1 || j = -1 then None
    else begin
      if i < j then Some ((i,j))
      else Some ((j,i))
    end)
  |> Array.to_list

let without_root ~graph =
  let e = normalize2 graph.e in 
  let chance eps = Float.(Random.float 1. <= eps) in
  let attempt eps =
    let in_tree = Array.init graph.v ~f:(fun _ -> false) in
    let next = Array.init graph.v ~f:(fun _ -> -1) in
    let num_roots = ref 0 in
    let u = ref (-1) in
    let i = ref 0 in
    let cont = ref true in 
    while !cont && !i < graph.v do 
      u := !i;
      while in_tree.(!u) |> not do
        if chance eps then begin 
          next.(!u) <- -1;
          in_tree.(!u) <- true;
          num_roots := !num_roots + 1;
          if !num_roots = 2 then
            cont := false 
        end else begin
          next.(!u) <- rand_succ e.(!u);
          u := next.(!u)
        end
      done;
      if !cont then begin 
        u := !i;
        while in_tree.(!u) |> not do
          in_tree.(!u) <- true;
          u := next.(!u)
        done
      end;
      i := !i + 1
    done;
    if !cont then Some (next) else None 
  in
  let rec loop eps =
    match attempt eps with
    | None -> (eps /. 2.) |> loop
    | Some next -> next
  in
  let next = loop 0.5 in
  Array.filter_mapi next ~f:(fun i j ->
    if i = -1 || j = -1 then None
    else begin
      if i < j then Some ((i,j))
      else Some ((j,i))
    end)
  |> Array.to_list
