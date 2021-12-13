open Core

type graph = {
  v : Int.t;
  e : ((Int.t * Float.t) List.t) Array.t;
}

let parse fname = 
  Stdio.In_channel.with_file fname ~f:(fun file ->
    let v = Stdio.In_channel.input_line_exn file |> Int.of_string in
    let e = Stdio.In_channel.input_line_exn file |> Int.of_string in
    let e_arr = Array.init v ~f:(fun _ -> []) in
    Fn.apply_n_times ~n:e (fun _ ->
      let line = Stdio.In_channel.input_line_exn file in
      match String.split line ~on:',' with
      | vi :: vj :: p :: [] ->
        let vi', vj', p' = Int.of_string vi, Int.of_string vj, Float.of_string p in
        e_arr.(vi') <- (vj',p') :: e_arr.(vi');
        e_arr.(vj') <- (vi',p') :: e_arr.(vj')
      | _ -> failwith "Edge description is wrong."
    ) ();
    { v = v; e = e_arr })
