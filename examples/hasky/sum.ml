sum :: Fun (List(Int), Int) 
  = fun (xs List(Int)) ->
    match xs with (
    | List.Nil()       -> 0
    | List.Cons(y, ys) -> y + sum(ys)
    )
  ;;

main :: Void = print(sum([1,2,3,4,5])) ;;
