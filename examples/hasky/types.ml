type Tree (a)
  = Leaf
  | Node (Tree(a), a, Tree(a))
;;

x :: Tree(Int) 
  = Tree.Node(
      Tree.Node(Tree.Leaf(), 1, Tree.Leaf()), 
      3, 
      Tree.Node(
        Tree.Node(Tree.Leaf(), 1, Tree.Leaf()),
        2,
        Tree.Leaf()
      )
    )
;;

main :: Void = print(x) ;;

(**)
(* map_tree :: (a, b) => Fun(Fun(a, b), Tree(a), Tree(b)) *)
(*   = fun (f Fun(a, b), tree Tree(a)) -> *)
(*     match tree with ( *)
(*     | Tree.Leaf() -> Tree.Leaf() *)
(*     | Tree.Node( l, x, r ) ->  *)
(*       let m :: Fun(Tree(a), Tree(b)) = map_tree(f) *)
(*       in Tree.Node ( m(l), f(x), m(r) ) *)
(*     ) *)
(* ;; *)
(**)
(* main :: Void = print(map_tree( *)
(*   fun (x Int) -> x * 2, *)
(*   Tree.Node( *)
(*       Tree.Node( Tree.Leaf(), 1, Tree.Leaf() ),  *)
(*       Bool.True(),                                        *)
(*       Tree.Node( Tree.Leaf(), 3, Tree.Leaf() ) *)
(*     ) *)
(* )) ;; *)
