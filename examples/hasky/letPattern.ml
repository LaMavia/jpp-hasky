main :: Void = 
  let Maybe.Just(x) :: Maybe(Int) = Maybe.Just(5) in
  let Maybe.Just([x]) :: Maybe(List(Bool)) = Maybe.Nothing in
  print(x)
;;
