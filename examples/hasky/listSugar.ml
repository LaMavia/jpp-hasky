myList :: List(Int) = [1, 2, 3, 4] ;;

listMatch :: Fun(List(Int), Int)
  = fun (xs List(Int)) ->
    match xs with (
    | [] -> 0
    | [a] -> a
    | [a, b] -> a + b
    )
;;
