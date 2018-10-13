(** (Pseudo-)Metric spaces. The axioms are not explicitly enforced. *)
module type S =
  sig
    type t
    val dist : t -> t -> float
  end

(** Finite (pseudo-)metric spaces. Elements are explicitly stored in an array. *)
module type FiniteS =
  sig
    include S
    val elements : t array
  end

(** Ordered (pseudo-)metric spaces. Useful to construct spaces of maps, etc. *)
module type OrderedS =
  sig
    include S

    val compare : t -> t -> int
  end

(** Computes the pullback of a (pseudo) metric space along a map. *)
val pullback : ('a -> 'b) -> (module S with type t = 'b) -> (module S with type t = 'a)
