(* Result: 1 *)
let hd (l: int list) =
  match l with
  | [] -> 0
  | h::t -> h
;;

hd (1::(2::[]))
;;
