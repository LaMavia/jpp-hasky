xs :: List (Int) = [1,2,3] ;;

x :: Int = 
  match xs with ( 
    | List.Cons (h, _) -> h 
  ) 
;;

tail_xs :: List (Int) = 
  match xs with (
    | List.Cons (_, tl) -> tl 
  ) 
;;

main :: Void 
  =  let _ :: Void = print(x)
  in let _ :: Void = print(tail_xs)
  in Void.Void()
;;
