eq :: (a) => Fun(a, a, Bool)
  = fun (x a, y a) -> x == y
;;

yup1 :: Bool = eq(1,1) ;;
yup2 :: Bool = eq([1, 2], [1, 2]) ;;
nop  :: Bool = eq([], [1]) ;;

