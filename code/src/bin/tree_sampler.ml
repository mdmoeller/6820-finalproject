open Core

type m =
  | Root of int
  | NoRoot
  | KruskalDet
  | KruskalRandSimp
  | KruskalRandProp
  | Test of int * int 

let fname = Command.Param.(flag "-fname" (required string) ~doc:"[filename/string] Name of file to read from.")

let root_type = Command.Arg_type.create (fun i -> Root (Int.of_string i))
let rwr = Command.Param.(flag "-wilson-root" (optional root_type) ~doc:"[root/int] Wilson with root.")
let rnr = Command.Param.(flag "-wilson-no-root" (no_arg_some NoRoot) ~doc:"Wilson with no root.")
let krk = Command.Param.(flag "-kruskal" (no_arg_some KruskalDet) ~doc:"Deterministic Kruskal.")
let krs = Command.Param.(flag "-kruskal-simple-random" (no_arg_some KruskalRandSimp) ~doc:"Simple randomized Kruskal.")
let krp = Command.Param.(flag "-kruskal-proportional-random" (no_arg_some KruskalRandProp) ~doc:"Proportional randomized Kruskal.")
let tst_type = Command.Arg_type.create (fun i ->
  match String.split i ~on:',' with
  | hd1 :: hd2 :: [] -> Test (Int.of_string hd1, Int.of_string hd2)
  | hd1 :: [] -> Test (Int.of_string hd1, 10)
  | _ -> failwith "Invalid paramter usage.")
let tst = Command.Param.(flag "-test" (optional tst_type) ~doc:"[iters,batch size/int*int] Run the tester with iters iteration and batches of size batch. It must be that iters % batch = 0.")

let mode = Command.Param.choose_one [rwr; rnr; krk; krs; krp; tst] ~if_nothing_chosen:Raise

let canonc res = List.map (List.stable_sort res ~compare:(Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare)) ~f:(fun (i,j) -> Printf.sprintf "(%d,%d)" i j)

let () =
  Command.basic
    ~summary:"Args for the program. (* fill this out later *)"
    [%map_open.Command
      let fname = fname
      and mode = mode
      in
      fun () ->
        let graph = Parser.parse fname in
        let res, out =
          match mode with
          | Root root -> Random_tree.with_root ~root ~graph |> canonc, Printf.sprintf "%s_wilson_root_out" fname
          | NoRoot -> Random_tree.without_root ~graph |> canonc, Printf.sprintf "%s_wilson_no_root_out" fname
          | KruskalDet -> Kruskal.det_kruskal ~graph |> canonc, Printf.sprintf "%s_det_kruskal_out" fname 
          | KruskalRandSimp -> Kruskal.simp_rand_kruskal ~graph |> canonc, Printf.sprintf "%s_simple_random_kruskal_out" fname
          | KruskalRandProp -> Kruskal.prop_rand_kruskal ~graph |> canonc, Printf.sprintf "%s_proportional_random_kruskal_out" fname
          | Test (iters, batch) -> Tester.test ~graph ~iters:iters ~batch:batch, Printf.sprintf "%s_test_out" fname
        in
        Stdio.Out_channel.with_file out ~f:(fun file -> Stdio.Out_channel.output_lines file res)]
  |> Command.run 
