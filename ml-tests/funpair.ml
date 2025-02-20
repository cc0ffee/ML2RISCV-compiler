(* Result: 4 *)
let p = ((fun (x: int) -> x + 1), (fun (x: int) -> x * 3))
    in
    let (f, g) = p in
    f (g 1)
;;
