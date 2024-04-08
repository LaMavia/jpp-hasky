(* defined in the stdlib *)
type Void = Void ;;

fact :: Fun (Int, Int) 
  = fun (n Int) -> 
    match n with (
    | 0 -> 1
    | n -> n * fact (n - 1)
    )
  ;;

main :: Void
  = print(fact(5)) (* prints "120" *)
;;
