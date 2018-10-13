(* Infinite streams (i.e. not finite) *)

type 'a t = { v : 'a; tl : unit -> 'a t }

let cons v tl = { v; tl }

let peek { v; _ } = v

let tl { tl; _ } = tl ()
