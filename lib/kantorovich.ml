open Owl

let mat_to_bigarray1 (m : Mat.mat) =
  let rows, cols = Mat.shape m in
  Bigarray.reshape_1 m (rows * cols)

let mat_to_bigarray2 (m : Mat.mat) =
  let rows, cols = Mat.shape m in
  Bigarray.reshape_2 m rows cols

module Make(X : Metric.FiniteS) =
  struct

    type t = Mat.mat

    let dist_mat =
      let num_elts = Array.length X.elements in
      let mat =
        Mat.init_2d num_elts num_elts (fun row col ->
            X.dist X.elements.(row) X.elements.(col)
          )
      in
      mat_to_bigarray2 mat

    let optimal_coupling x y max_iter =
      Camlot.kantorovich ~x ~y ~d:dist_mat ~num_iter:max_iter

    let dist x y =
      let rec loop max_iter =
      match optimal_coupling x y max_iter with
        | MaxIterReached _ ->
          loop (max_iter * 2)
        | Optimal { cost; _ } -> cost
        | Unbounded
        | Infeasible ->
          failwith "Kantorovich.Make.dist: unbounded or unfeasible transport problem"
      in
      loop 100

  end
