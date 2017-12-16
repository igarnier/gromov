module R2 : 
  sig
    
    type t = { x : float; y : float }

    val dist : t -> t -> float

  end

val lp : float -> (module Metric.S with type t = Owl.Dense.Vector.D.vec)
