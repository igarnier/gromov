module Bounded : functor (X : Metric.S) -> Metric.S with type t = X.t =
  functor (X : Metric.S) ->
    struct

      type t = X.t

      let dist x y =
        let d = X.dist x y in
        d /. (1. +. d)

    end

type ('a, 'b) sum =
  | Inl of 'a
  | Inr of 'b

module Sum : functor (X : Metric.S) (Y : Metric.S) -> Metric.S with type t = (X.t,Y.t) sum =
  functor (X : Metric.S) (Y : Metric.S) ->
  struct

    module Xb = Bounded(X)
    module Yb = Bounded(Y)
    
    type t = (Xb.t, Yb.t) sum

    let dist a b =
      match a, b with
      | Inl x, Inl x' -> Xb.dist x x'
      | Inr y, Inr y' -> Yb.dist y y'
      | _             -> 2.0

  end

module Tensor = Tensor
