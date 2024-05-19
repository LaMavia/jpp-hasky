myList :: List(Int) = [1, 2, 3, 4] ;;

listMatch :: Fun(List(Int), Int)
  = fun (xs List(Int)) ->
    match xs with (
    | [a] -> a
    | [a, b] -> a + b
    | [a, b, c] -> a*b + c
    )
;;

x :: Void = print(listMatch([0])) ;;
y :: Void = print(listMatch([1, 2])) ;;
z :: Void = print(listMatch([10, 3, 7])) ;;
