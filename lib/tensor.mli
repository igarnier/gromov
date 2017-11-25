module L1 : functor (X : Metric.S) (Y : Metric.S) -> Metric.S with type t = X.t * Y.t

module L2 : functor (X : Metric.S) (Y : Metric.S) -> Metric.S with type t = X.t * Y.t

module Lp : functor (P : sig val p : float end) (X : Metric.S) (Y : Metric.S) -> Metric.S with type t = X.t * Y.t

module Linfty : functor (X : Metric.S) (Y : Metric.S) -> Metric.S with type t = X.t * Y.t

module type PowParams =
sig 
  val n : int 
  val norm : [ `Linfty | `Lp of float ] 
end

module Pow : functor (P : PowParams) (X : Metric.S) -> Metric.S with type t = X.t array

module InfPow : functor (X : Metric.S) -> 
sig
  type t = X.t InfList.t

  val dist : int -> (t -> t -> float)
end
