(* Result: 8 *)
let rec fib (n: int): int =
  if n <= 1 then n
  else fib (n - 2) + fib (n - 1)
;;

fib 6;;
