type ('a, 'b) tensor = 
  (module Metric.S with type elt = 'a) ->
  (module Metric.S with type elt = 'b) ->
  (module Metric.S with type elt = 'a * 'b)

type holder = [ `Linfty | `Lp of float ]

val l1 : ('a, 'b) tensor
val l2 : ('a, 'b) tensor
val lp : holder -> ('a, 'b) tensor
val pow : int -> holder -> (module Metric.S with type elt = 'a) -> (module Metric.S with type elt = 'a array)
(* val infpow : (module Metric.S with type elt = 'a) -> (module Metric.S with type elt = 'a InfList.t) *)
