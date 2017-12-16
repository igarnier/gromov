open Owl

module V = Dense.Vector.D
module M = Dense.Matrix.D

(* 2d eucldidian space*)
module R2 =
struct

  type t = { x : float; y : float }

  let dist a b =
    let dx = b.x -. a.x in
    let dy = b.y -. a.y in
    sqrt (dx *. dx +. dy *. dy)

end

let lpnorm p vec =
  let d = V.numel vec in
  let v = M.pow vec (V.create d p) in
  let s = M.sum v in
  (M.get s 0 0) ** (1.0 /. p)

let lp : float -> (module Metric.S with type t = V.vec) =
  fun p ->
    let module M =
      struct

        type t = V.vec
        
        let dist a b = lpnorm p (V.sub b a)

      end
    in
    (module M)
