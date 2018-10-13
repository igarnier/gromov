
module type S =
  sig
    type t
    val dist : t -> t -> float
  end

module type FiniteS =
  sig
    include S
    val elements : t array
  end

module type OrderedS =
  sig
    include S
    val compare : t -> t -> int
  end

let pullback (type a b) (f : a -> b) (met : (module S with type t = b)) =
  let module Met = (val met) in
  let module PbMet =
  struct
    type t = a

    let dist x y =
      Met.dist (f x) (f y)
  end
  in
  (module PbMet : S with type t = a)
