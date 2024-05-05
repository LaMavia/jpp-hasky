type T (a) = A ;; (* OK *)
type U = B ( T(Void) ) ;; (* OK *)
type V = X ( U, T(a, b, c) ) ;; (* FAILS *)
