sub :: Fun(Int, Int, Int)
  = fun (x Int, y Int) ->
    let _ :: Void = print(x, y, Void.Void()) in
    x - y
;;

subs :: Fun(List(Int), Int)
  = fun (xs List(Int)) ->
    match xs with (
      | List.Nil() -> 0
      | List.Cons(h, tl) ->
        foldr(sub, h, tl)
    )
;;

main :: Void = print(subs([1,2,3,4])) ;;
