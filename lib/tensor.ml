(* Binary, finite anf infinite products of metric spaces. *)

open Batteries

module Dists =
struct

  type vec     = float array
  type norm    = vec -> float
  type 'a t = 'a -> 'a -> float

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

  let binary : 'a 'b. ('a t) -> ('b t) -> norm -> ('a * 'b) t =
    fun tx ty norm (x1,y1) (x2,y2) ->
      norm [| tx x1 x2; ty y1 y2 |]

  (* We are forced to pick the same type for all ts.*)
  let nary : 'a. ('a t) array -> norm -> 'a array t =
    fun ts norm x y ->
      norm (Array.mapi (fun i t -> t x.(i) y.(i)) ts)

  (* let infty : 'a.  ('a t) InfList.t -> int -> ('a InfList.t) t =
   *   fun ts precision x y ->
   *     let rec loop n ts x y acc =
   *       if n = precision then acc
   *       else
   *         let t = InfList.peek ts in
   *         let xt = InfList.peek x in
   *         let yt = InfList.peek y in
   *         let d    = t xt yt in
   *         let pow  = float Int.(2 ** n) in
   *         let acc  = acc +. (d /. (pow *. (1. +. d))) in
   *         loop (n+1) (InfList.tl ts) (InfList.tl x) (InfList.tl y) acc
   *     in
   *     loop 1 ts x y 0.0 *)

end

type ('a, 'b) tensor = 
  (module Metric.S with type t = 'a) ->
  (module Metric.S with type t = 'b) ->
  (module Metric.S with type t = 'a * 'b)

type holder = [ `Linfty | `Lp of float ]


let l1 (type t1 t2) (module X : Metric.S with type t = t1) (module Y : Metric.S with type t = t2) =
  (module struct

    type t = X.t * Y.t

    let dist = Dists.binary X.dist Y.dist Dists.l1

  end : Metric.S with type t = t1 * t2)

let l2 (type t1 t2) (module X : Metric.S with type t = t1) (module Y : Metric.S with type t = t2) =
  (module struct

    type t = X.t * Y.t

    let dist = Dists.binary X.dist Y.dist Dists.l2

  end : Metric.S with type t = t1 * t2)

let lp (type t1 t2) holder (module X : Metric.S with type t = t1) (module Y : Metric.S with type t = t2) =
  (module struct

    type t = X.t * Y.t

    let dist = 
      match holder with
      | `Linfty ->
        Dists.binary X.dist Y.dist Dists.linfty
      | `Lp p ->
        Dists.binary X.dist Y.dist (Dists.lp p)

  end : Metric.S with type t = t1 * t2)

let pow (type t) n holder (module X : Metric.S with type t = t) =
  (module struct

    type t = X.t array

    let dist =
      match holder with
      | `Lp 1.0 ->
        let arr = Array.create n X.dist in
        Dists.nary arr Dists.l1
      | `Lp 2.0 ->
        let arr = Array.create n X.dist in
        Dists.nary arr Dists.l2
      | `Lp p ->
        let arr = Array.create n X.dist in
        Dists.nary arr (Dists.lp p)
      | `Linfty ->
        let arr = Array.create n X.dist in
        Dists.nary arr Dists.linfty

  end : Metric.S with type t = X.t array)

(* let infpow (type t) (module X : Metric.S with type t = t) =
 *   (module struct
 * 
 *     type t = X.t InfList.t
 *         
 *     let dist = 
 *       let rec d () = InfList.cons X.dist d in
 *       Dists.infty (d ())
 *         
 *   end : Metric.S with type t = X.t InfList.t) *)
