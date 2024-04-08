# Hasky

Język funkcyjny inspirowany haskellem oraz ocaml-em. 


### Dane autorki

Zuzanna Surowiec

nr. albumu: 438730



## Konstrukcje językowe

### Typy

Język wymaga adnotacji typowych przy:
1. argumentach funkcji.
    ```ml
    fact :: Fun(Int, Int) 
        = fun (n Int) -> (* ... *)
    ;;
    ```

Wbudowane są typy:

1. `Fun(arg1, ..., argN, returnType)`. Dla przykładu: `Fun(Int, Int, Int)` jest równoważny Haskellowemu `Int -> Int -> Int`. W szczególności, `Fun(a) = a` dla każdego typu `a`.
2. `Bool` zadeklarowany jako
    ```ml
    type Bool = True | False ;;
    ```
3. `Void` zadeklarowany jako
    ```ml
    type Void = Void ;;
    ```
4. `List(a)` zadeklarowany jako
    ```ml
    type List(a) 
        = Nil
        | Cons(a, List(a))
    ;;
    ```

Możliwe są deklaracje typów własnych, w tym typów generycznych za pomocą deklaracji `type` w pierwszej warstwie programu (top level). Typy oraz ich konstruktory nazywane są z dużych liter, a zmienne typowe - z małych.

1. Generyczny typ 
    ```ml
    type Tree(a) 
        = Leaf
        | Node( Tree(a), a, Tree(a) )
    ;;
    ```
2. Niegeneryczne typy
    ```ml
    type MyType = Constr1(Type1, ..., TypeN) | ... | ConstrM(Type1, ..., TypeK) ;;
    ```



### Deklaracje górnopoziomowe

Deklaracje te działają podobnie do deklaracji haskellowych, ale z jedną różnicą: identyfikatory dostępne są dopiero po ich zdefiniowaniu. Dla przykładu, w haskellu:

```hs
f y = x + y

x :: Integer
x = 5
```

Funkcja `f` widzi zmienną `x = 5`, mimo że jej definicja następuje po deklaracji funkcji `f`. W naszym języku, `x` musiałaby była zostać zdefiniowana przed deklaracją funkcji `f`.

Język nasz nie posiada również lukru syntaktycznego na definicje funckji w postaci `f x y = ...`. Pisane muszą być więc eksplisite przypisania do wyrażeń funkcyjnych:
```ml
inc :: Fun(Int, Int) = fun(x Int) -> x + 1 ;;
```
Dostępna jest również wersja bez podania typu deklaracji:
```ml
inc = fun(x Int) -> x + 1 ;;
```



### Przypisania let

Przypisania postaci `let x :: Type = value in expression`, będące wyrażeniemi. Działają jak haskellowe. Wszelkie konflikty nazw rozwiązywane są przysłanianiem.
Dostępne jest również `let x = value in expression` z rekonstrukcją typów.



### Wyrażenia funkcyjne

Wyrażenia będące odpowiednikiem haskellowej lambdy w postaci
```ml
fun(arg1 Type1, ..., argN TypeN) -> expression(arg1, ...,  argN)
```

Typy argumentów są obowiązkowe. Lista agrumentów nie może być pusta (jak to ma miejsce w Haskellu), gdyż `Fun(a) = a` dla dowolnego typu `a` (rownoważnie w Haskellu, przez binarność konstruktora `(->) :: * -> * -> *` i brak unarnego odpowiednika).



### Wyrażenia warunkowe

Dokładnie jak haskellowe: `if condition_expression then then_expression else else_expression`.



### Pattern matching

Obsługiwane są dowolnie zagnieżdżone wzorce. Pattern matching obowiązuje wyłącznie przy przypisaniach `let` oraz wyrażeniach `match expression with (...)`. Np.:
```ml
x :: Int = 5 ;;
is_x_even :: Bool = match 5 % 2 with (
    | 0 -> True
    | 1 -> False
    | something_else -> (* ... *)
    ) ;;
```

Wzorce dopasowywane są na zasadzie usgadniania termów - wartości wyrażenia i wyrażeń wzorców.

Ignorowane są przypisania do termu `_`.

Jeśli żaden ze wzorców nie zostanie dopasowany, rzucany jest wyjątek o tym informujący.



### Listy

Notacja haskellowa: `[ a, b, c ]`, która jest lukrem dla `Cons(a, Cons(b, Cons(c, Nil))).
Pattern matching z użyciem `Cons`, `Nil` lub lukru.



### Arytmetyka i porównania

Jak haskellowe.



### Komentarze

Jednolinijkowe: `# komentarz`.

Wielolinijkowe: `(* komentarz *)`.



## Rekonstrukcja typów

### Zmienne typowe

Wszelkie napotkane, niezdefiniowane w obecnym środowisku zmienne typowe są traktowane, jakby były zadeklarowane w kolenjości leksykograficznej. Np.
```ml
f = fun (x a, y b) -> 1 ;;
```
jest równoważne
```ml
f :: (a, b) => Fun(a, b, Int) 
  = fun (x a, y b) -> 1 ;;
```

Zmienne typowe nie są jednak uzgadniane. Tj. 
```ml
concat 
  = fun (xs List(a), ys List(b)) -> 
    match xs with (
    | Nil -> ys
    | Cons(x, xs) -> Cons(x, concat(xs, ys))
    )
;;
```
nie jest poprawnym otypowaniem, gdyż nie ustalimy `a === b`.

## Bilbioteka standardowa

### Typy

1. **Fun** 
  Typ wewnętrzny.
  `f : s1 -> ... -> sn -> s` odpowiada `Fun(s1, ..., sn, s)`.

2. **Bool**
  ```ml
  type Bool = True | False ;;
  ```

3. **Int** 
  Typ wewnętrzny. Jak w Haskellu.

4. **Void**
  ```ml
  type Void = Void ;;
  ```
  
2. **List** 
  ```ml
  type List(a)
    = Nil
    | Cons(a, List(a))
  ;;
  ```

3. **Maybe**
  ```ml
  type Maybe(a)
    = Nothing
    | Just(a)
  ;;
  ```



### Funkcje

1. **head**
  ```ml
  head :: (a) => Fun(List(a), a)
  ```

2. **tail**
  ```ml
  tail :: (a) => Fun(List(a), List(a))
  ```

3. **empty**
  ```ml
  empty :: (a) => Fun(List(a), Bool)
  ```

4. **map**
  ```ml
  map :: (a, b) => Fun(Fun(a, b), List(a), List(b))
  ```


### Operatory

1. `(+) :: Fun(Int, Int, Int)`
2. `(-) :: Fun(Int, Int, Int)`
3. `(*) :: Fun(Int, Int, Int)`
4. `(/) :: Fun(Int, Int, Int)` (dzielenie całkowite: `div` haskellowe)
5. `(%) :: Fun(Int, Int, Int)` (operator modulo)



## Tabelka cech

  Na 20 punktów
+ 01 (dwa typy)
+ 02 (arytmetyka, porównania)
+ 03 (if)
+ 04 (funkcje wieloargumentowe, rekurencja)
+ 05 (funkcje anonimowe i wyższego rzędu, częściowa aplikacja)
+ 06 (obsługa błędów wykonania)
+ 07 (statyczne wiązanie identyfikatorów)

  Listy:
+ 08 (z pattern matchingiem)
+ 09 (z empty, head, tail)
+ 10 (lukier)

  Na 25 punktów
+ 11 (listy dowolnego typu, zagnieżdżone i listy funkcji)
+ 12 (proste typy algebraiczne z jednopoziomowym pattern matchingiem)
+ 13 (statyczne typowanie)

  Na 30 punktów
+ 14 (ogólne polimorficzne i rekurencyjne typy algebraiczne)
+ 15 (zagnieżdżony pattern matching)

  Bonus
+ 16 (częściowa rekonstrukcja typów)
Razem: 32
