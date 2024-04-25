fact :: Fun(Int, Int) =
  fun (n Int) ->
    let aux :: Fun(Int, Int, Int) 
      = fun (n Int, u Int) ->
        if n <= 1 then 
          u 
        else 
          aux(n - 1, n * u)
    in aux(n, 1)
  ;;

z :: Int = fact(5) ;;
main :: Void = print(z) ;;

