(* defined in the stdlib *)
type List (a) 
  = Nil 
  | Cons (a, List(a))
;;

type Bool
  = True
  | False
;;

empty :: (a) => Fun(List(a), Bool) 
  = fun (x List(a)) ->
    match x with (
      | List.Nil -> Bool.True
      | _        -> Bool.False
    )
;;

head :: (a) => Fun(List(a), a) 
  = fun (x List(a)) ->
    match x with (
      | List.Cons (h, _) -> h
    )
;;

tail :: (a) => Fun(List(a), List(a))
  = fun (x List(a)) ->
    match x with (
      | List.Cons(_, t) -> t
    )
;;

(* an examplt of sugar pattern matching *)
isSingleton :: Fun(List(Int), Bool)
  = fun (xs List(Int)) ->
    match xs with (
      | [_] -> Bool.True
      |  _  -> Bool.False
    )
;;
