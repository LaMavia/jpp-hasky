type Bool = True | False ;;

not :: Fun (Bool, Bool) 
  = fun(x Bool) -> 
    match x with (
    | False -> True
    | True  -> False
    )
  ;;

