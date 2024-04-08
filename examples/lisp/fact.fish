(def-fun fact (-> Int Int)
  (0 Int. 1)
  (n Int. (* n (fact (- n 1))))
)


