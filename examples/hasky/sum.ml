sum :: Fun (List(Int), Int) 
  = fun (xs List(Int)) ->
    match xs with (
    | Nil          -> 0
    | Cons (y, ys) -> y + add(ys)
    )
  ;;

