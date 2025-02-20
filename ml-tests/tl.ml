(* Result: 2 *)
(* When you add something to the environment between generating an expression
 * and using it, you have to make sure your deBruijn indices are still right. *)
let l = 1::(2::[]) in
match l with
| [] -> 0
| fst::t ->
   (match t with
    | [] -> 0
    | snd::t -> snd)
;;
