module R2 : 
  sig
    
    type elt = { x : float; y : float }

    val dist : elt -> elt -> float

  end

val lp : float -> (module Metric.S with type elt = Owl.Dense.Vector.D.vec)
