# macros work directly on expressions (of type Expr), 
# and return an expression. Nothing is evaluated.
(macro let 
  [x t v e]
  ((x t. e) v)
)

(let x Int 5 (print x))
# is the same as
((x Int. (print x)) 5) 
