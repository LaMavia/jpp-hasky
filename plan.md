# Atoms to implement

## def

### Signature

```fish
(def identifier type value)
```

### Description

Set the value of `identifier` of type `type` to the value of `value` in the parent scope. Returns a value of type `Void`.

### Examples

```fish
(def x Int 5)
x # == 5
```


## def-type

### Signature 

```fish
(def-type Type-Name [T1 T2 ... Tn]
    (constructor1 (type1))
    # ...
    (constructorm (typem))
)
```

### Description

Defines a type `Type-Name` with type variables `T1, ..., Tn`, and
constructors `constructor1` of type `type1`, ..., `constructorm` of type `typem` 
in the parent scope. Returns a value of type `Void`.

### Examples

```fish
(def-type Void [] 
    (void Void)
)
```

```fish
(def-type Tree [T]
  (leaf (Tree T))
  (node (-> (Tree T) T (Tree T) (Tree T))
  )
)

(def-fun inc-tree (-> (Tree Int) (Tree Int))
    (`leaf. 
        leaf)
    ((`node l v r). 
        (node 
            (inc-tree l) 
            (+ v 1) 
            (inc-tree r)
        ))
)
```

## def-fun

### Signature

```fish
(def-fun f t c1 ... cn)

```



# Constructs

## Quotation

Treats its argument as a raw term, and returns it unchanged. For instance, given a function `inc :: Int -> Int`

```fish
(def-fun inc (-> Int Int) 
    (n. (+ n 1))
)
```

an expression `(inc 5)` evaluates to `5`, but `` `(inc 5) `` evaluates to `(inc 5)`.
In the same manner, 

```fish
`(inc (inc 5)) # == (inc (inc 5))
`(inc `(inc 5)) # == (inc `(inc 5))
```

