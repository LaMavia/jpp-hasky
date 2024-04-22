type Bool = True | False ;;

not :: Fun (Bool, Bool) 
  = fun(x Bool) -> 
    match x with (
    | Bool.False -> Bool.True
    | Bool.True  -> Bool.False
    )
  ;;

