x :: Int =
  let secret :: Int = 42 in 5
;;

main :: Void = 
  let _ :: Void = print(x) (* OK *)
  in print(secret)
;;


