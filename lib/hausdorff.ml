(* Various implementations of the Hausdorff distance between subsets of a metric
   space. Here, we only consider finite subsets. The implementations vary in the 
   data structure used to represents subsets: balanced trees, lists, arrays, etc. 
   Some data structures might require more hypothesis on the structure of the
   underlying space. *)

open Batteries

module ArrayBased : functor (X : Metric.S) -> Metric.S with type elt = X.elt array =
  functor (X : Metric.S) ->
  struct
    
    type elt = X.elt array
        
    let dist_points set1 set2 i1 i2 =
      let p1 = set1.(i1) in
      let p2 = set2.(i2) in
      X.dist p1 p2

    let inf set1 set2 i1 =
      let acc = ref max_float in
      for i2 = 0 to (Array.length set2) - 1 do
        let v = dist_points set1 set2 i1 i2 in
        if v < !acc then
          acc := v
      done;
      !acc

    let sup_inf set1 set2 =
      let acc = ref (~-. max_float) in
      for i1 = 0 to (Array.length set1) - 1 do
        let v = inf set1 set2 i1 in
        if v > !acc then
          acc := v
      done;
      !acc

    let dist set1 set2 =
      max (sup_inf set1 set2) (sup_inf set2 set1)

  end
