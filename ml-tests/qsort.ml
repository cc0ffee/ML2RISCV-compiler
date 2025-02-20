(* Result: 1 *)
let rec merge (l1l2 : int list * int list) : int list =
  let (l1, l2) = l1l2 in
  match l1 with
  | [] -> l2
  | h1::t1 ->
     (match l2 with
      | [] -> l1
      | h2::t2 ->
         (if h1 < h2 then
            (h1::(merge (t1, l2)))
          else
            h2::(merge (l1, t2))
         )
     )
;;

let partition (p: int) =
  let rec part_rec (l: int list) : int list * int list =
    match l with
    | [] -> (([] : int list), ([] : int list))
    | h::t ->
       let (le, gt) = part_rec t in
       if h <= p then (h::le, gt)
       else (le, h::gt)
  in
  part_rec
;;

let rec qsort (l: int list) : int list =
  match l with
  | [] -> ([]: int list)
  | p::t ->
     let (le, gt) = partition p t in
     let (sle, sgt) = (qsort le, qsort gt) in
     merge (sle, p::sgt)
;;

let l : int list = 5::2::1::8::[]
;;

match qsort l with
| [] -> 0
| h::t -> h
;;
