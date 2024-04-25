add :: Fun(Int, Int, Int) 
  = fun (x Int, y Int) -> 
    x + y
;;

add5 :: Fun(Int, Int) = add(5) ;;

main :: Void = print(add5(-1)) ;;
