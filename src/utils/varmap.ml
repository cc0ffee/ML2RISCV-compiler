module M = Map.Make
               (struct
                 type t = string
                 let compare = String.compare
               end)

include M
