f :: Fun(Int, Void) =
  fun (x Int) ->
  if x == 0 then
    let y :: Int = 42 in f(1)
  else
    print(y) (* check if the previous frame 
                doesn't leak into calls 
              *)
;;

main :: Void = f(0) ;;
