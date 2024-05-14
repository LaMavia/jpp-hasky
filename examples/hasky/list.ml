empty :: (a) => Fun(List(a), Bool) 
  = fun (x List(a)) ->
    match x with (
      | List.Nil() -> Bool.True
      | _          -> Bool.False
    )
;;

head :: (a) => Fun(List(a), a) 
  = fun (x List(a)) ->
    match x with (
      | List.Cons(h, _) -> h
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

main :: Void 
  =  let _ :: Void = print([empty([]), empty([1,2,3])])
  in let _ :: Void = print([head([1,2,3])])
  in let _ :: Void = print([tail([1,2,3])])
  in let _ :: Void = print([isSingleton([1,2,3]), isSingleton([1])])
  in Void.V
;;
