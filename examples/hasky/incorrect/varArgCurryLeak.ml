f :: Fun(Int, Int, Int)
  = fun (secret Int, y Int) -> y
;;

main :: Void
  = let adder :: Fun(Int, Int) = f(42) in print(secret)
;;
