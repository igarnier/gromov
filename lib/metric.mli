module type S =
  sig
    type t
    val dist : t -> t -> float
  end

module type OrderedS =
  sig
    include S

    val compare : t -> t -> int
  end
