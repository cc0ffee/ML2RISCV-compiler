(* Result: 8 *)
let uncurry (f: int -> int -> int) =
  fun (xy: int * int) -> let (x, y) = xy in f x y
;;

let add = fun (x: int) -> fun (y: int) -> x + y
;;

uncurry add (5, 3)
;;
