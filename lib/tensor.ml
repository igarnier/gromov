(* Binary, finite anf infinite products of metric spaces. *)

open Batteries
 
type vec     = float array
type norm    = vec -> float
type 'a dist = 'a -> 'a -> float

(* We assume throughout that all arrays contain positive values. *)
let l1 : norm = Array.fsum

type r = { mutable r : float }

let linfty : norm =
  fun arr ->
    let x = { r = 0.0 } in
    for i = 0 to Array.length arr - 1 do
      if arr.(i) > x.r then
        x.r <- arr.(i)
    done;
    x.r

  let l2 : norm = fun arr ->
    let arr = Array.map (fun x -> x *. x) arr in
    sqrt (Array.fsum arr)
      
  let lp : float -> norm =
    fun p arr ->
      let arr = Array.map (fun x -> x ** p) arr in
      (Array.fsum arr) ** (1. /. p)

let binary_tensor : 'a 'b. ('a dist) -> ('b dist) -> norm -> ('a * 'b) dist =
  fun distx disty norm (x1,y1) (x2,y2) ->
    norm [| distx x1 x2; disty y1 y2 |]

(* We are forced to pick the same type for all elts.*)
let nary_tensor : 'a. ('a dist) array -> norm -> 'a array dist =
  fun dists norm x y ->
    norm (Array.mapi (fun i dist -> dist x.(i) y.(i)) dists)

let infty_tensor : 'a.  ('a dist) InfList.t -> int -> ('a InfList.t) dist =
  fun dists precision x y ->
    let rec loop n dists x y acc =
      if n = precision then acc
      else
        let dist = InfList.peek dists in
        let xelt = InfList.peek x in
        let yelt = InfList.peek y in
        let d    = dist xelt yelt in
        let pow  = float Int.(2 ** n) in
        let acc  = acc +. (d /. (pow *. (1. +. d))) in
        loop (n+1) (InfList.tl dists) (InfList.tl x) (InfList.tl y) acc
    in
    loop 1 dists x y 0.0
          
      
module L1 : functor (X : Metric.S) (Y : Metric.S) -> Metric.S with type t = X.t * Y.t =
  functor (X : Metric.S) (Y : Metric.S) ->
    struct
      
      type t = X.t * Y.t

      let dist = binary_tensor X.dist Y.dist l1

    end

module L2 : functor (X : Metric.S) (Y : Metric.S) -> Metric.S with type t = X.t * Y.t =
  functor (X : Metric.S) (Y : Metric.S) ->
    struct
      
      type t = X.t * Y.t

      let dist = binary_tensor X.dist Y.dist l2

    end

module Lp : functor (P : sig val p : float end) (X : Metric.S) (Y : Metric.S) -> Metric.S with type t = X.t * Y.t =
  functor (P : sig val p : float end) (X : Metric.S) (Y : Metric.S) ->
    struct
      
      type t = X.t * Y.t

      let dist = binary_tensor X.dist Y.dist (lp P.p)

    end

module Linfty : functor (X : Metric.S) (Y : Metric.S) -> Metric.S with type t = X.t * Y.t =
  functor (X : Metric.S) (Y : Metric.S) ->
    struct
      
      type t = X.t * Y.t

      let dist = binary_tensor X.dist Y.dist linfty

    end

module type PowParams =
  sig

    val n : int

    val norm : [`Lp of float | `Linfty]

  end

module Pow : functor (P : PowParams) (X : Metric.S) -> Metric.S with type t = X.t array =
  functor (P : PowParams) (X : Metric.S) ->
    struct

      type t = X.t array

      let dist =
        match P.norm with
        | `Lp 1.0 ->
          let arr = Array.create P.n X.dist in
          nary_tensor arr l1
        | `Lp 2.0 ->
          let arr = Array.create P.n X.dist in
          nary_tensor arr l2
        | `Lp p ->
          let arr = Array.create P.n X.dist in
          nary_tensor arr (lp p)
        | `Linfty ->
          let arr = Array.create P.n X.dist in
          nary_tensor arr linfty

    end

module InfPow =
  functor (X : Metric.S) ->
  struct

    type t = X.t InfList.t

    let dist = 
      let rec d () = InfList.cons X.dist d in
      infty_tensor (d ())

  end
