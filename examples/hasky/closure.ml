addSecret :: Fun(Int, Int)
  = let secret :: Int = 17 
    in fun(x Int) -> x + secret
;;

main :: Void = print(addSecret(3)) ;;

