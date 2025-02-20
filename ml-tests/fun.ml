(* Result: 4 *)
let inc (x: int) = 1 + x in
    let dec = fun (x: int) -> x - 1 in
    (inc 1) + (dec 3)
;;
