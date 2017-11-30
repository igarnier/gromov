module V = Lacaml.D.Vec


(* 2d eucldidian space*)
module R2 =
struct

  type t = { x : float; y : float }

  let dist a b =
    let dx = b.x -. a.x in
    let dy = b.y -. a.y in
    dx *. dx +. dy *. dy

end

let lpnorm p vec =
  let d = V.dim vec in
  let v = V.pow vec (V.make d p) in
  let s = V.sum v in
  s ** (1.0 /. p)

let lp : float -> (module Metric.S with type t = V.t) =
  fun p ->
    let module M =
      struct

        type t = V.t
        
        let dist a b = lpnorm p (V.sub b a)

      end
    in
    (module M)
