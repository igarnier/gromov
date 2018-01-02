module type S =
  sig
    type elt
    val dist : elt -> elt -> float
  end

module type Finite =
  sig
    include S

    type t

    val card : t -> int

    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  end
