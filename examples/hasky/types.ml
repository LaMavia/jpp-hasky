type Tree (a)
  = Leaf
  | Node (Tree(a), a, Tree(a))
;;

map_tree 
  = fun (f Fun(a, b), tree Tree(a)) ->
    match tree with (
    | Leaf -> Leaf
    | Node ( l, x, r ) -> 
      let m = map_tree(f)
      in Node ( m(l), f(x), m(r) )
    )
;;

