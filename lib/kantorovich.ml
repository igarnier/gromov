open Batteries

module Make(X : Metric.OrderedS) =
  struct

    module M = Map.Make(X)

    type t = float M.t

  end
