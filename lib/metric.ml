module type S =
  sig
    type t
    val dist : t -> t -> float
  end
