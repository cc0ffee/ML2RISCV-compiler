(* Result: 3 *)
let add = fun (x: int) -> fun (y: int) -> x + y in
    let inc = add 1 in
    inc 2
;;
