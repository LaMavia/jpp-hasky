# defined in the stdlib
(def-type Tree [T]
  (leaf (Tree T))
  (node (-> (Tree T) T (Tree T) (Tree T)))
)

(def-type Bool []
  (true  Bool)
  (false Bool)
  (and   (-> Bool Bool Bool))
  (or    (-> Bool Bool Bool))
  (not   (-> Bool Bool))
)

