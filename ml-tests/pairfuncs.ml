(* Result: 25 *)

let square (n: int): int = n * n
;;

let square_pair (p: int * int) =
  let (fst, scd) = p in
  (fst * fst, scd * scd)
;;

let sum_pair (p: int * int) = 
  let (fst, scd) = p in
  fst + scd
;;

let p = (3, 4) in 
let p2 = square_pair p in 
sum_pair p2
;;