# defined in stdlib
# (def-type List [T]
#   (nil (List T))
#   (: (-> T (List T) (List T))
#   )
# )

(def-fun add (-> (List Int) Int)
  (nil      (List Int). 0)
  ((: x xs) (List Int). (+ x (add xs)))
)


