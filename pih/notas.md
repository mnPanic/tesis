# Notas de Programming in Haskell de Graham Hutton

Why Functional Programming Matters

> functional programming can be viewed as a style of programming in which the
> basic method of computation is the application of functions to arguments

Recomienda leer parte I entera, y después seleccionar los tópicos de la parte II
que sean de interés. Vital ir escribiendo código mientras se hace follow-along.

## 1 - Introduction

Contenidos:

- Noción de función
- Programación funcional
- Features principales de haskell
- Tres ejemplos chiquitos

### Funciones

Mapeos de uno o más argumentos a un resultado único. Se define con una ecuación,
ej.

```haskell
double x = x + x
```

Cuando se **aplica** una función a argumentos, el resultado se obtiene
sustituyendo los argumentos en el cuerpo de la función. Esto puede producir un
resultado que no se puede simplificar más, pero usualmente el resultado es una
*expresión* que contiene otras aplicaciones a funciones que deben ser
procesadas.

Para procesar `double (double 2)` hay dos formas, primero hacer el inner y
después el outer, o primero hacer el outer y después el inner. El resultado es
el mismo pero una tiene más pasos que la otra (porque se duplica el cómputo de
`double 2` cuando se hace la de afuera primero). Esto puede hacer que algunos
cómputos no terminen, que se vé más en detalle en el chapter 15.

### Programación funcional

Es un *estilo* de programación en donde el método básico de cómputo es la
aplicación de funciones a argumentos. Un lenguaje de programación funcional no
es más que uno que *soporta* y *alienta* ese estilo.

Ejemplo: computar la suma de los números de 1 a n

En java sería

```java
int total = 0;
for (int count = 1; count <= n; count++)
  total = total + count;
```

El método de cómputo es *changing stored values*, en el sentido de que la
ejecución del programa resulta en una secuencia de asignaciones. Por ejemplo,
con n = 3

```java
total = 0;
count = 1;
total = 1;
count = 2;
total = 3;
count = 3;
total = 6;
```

en general los lenguajes con este modelo de cómputo se llaman **imperativos**,
porque los programas se construyen de instrucciones imperativas que especifican
precisamente cómo debería proceder el cómputo.

En Haskell, se haría usando dos lib functions `sum [1..n]`. Acá el método de
cómputo principal es *aplicar funciones a argumentos*: la ejecución del programa
resulta en una secuencia de aplicaciones.

Muchos lenguajes imperativos permiten programar de esta forma, pero no lo
alientan.

### Features de haskell

- Programas concisos
- Type system poderoso, con inferencia de tipos.
- Funciones genéricas
- Evaluación lazy
- Equational reasoning

## 2 - First Steps

La standard library de haskell es un archivo que se llama *standard prelude*

Algunas sobre listas

- `head`
- `tail`
- `!!` (indexar el n-esimo)
- `take`
- `drop`
- `length`
- `sum`
- `product`
- `++` (append)
- `reverse`

Function application no usa paréntesis para reflejar su status central en el
lenguaje. También es la operación con mayor precedencia, por lo que

```haskell
f a + b
-- se interpreta como
(f a) + b
-- en lugar de
f (a + b)
```

Nota: hay veces en donde son necesarios los paréntesis, por ej. en `f (g x)`
donde sino `f g x` tomaria a g y x como argumentos de f.

> Interpretación mía: entonces lo que hace `$` es indicar al parser que terminó
> la lista de argumentos de f, y de ahí en adelante tomarlo como argumentos de
> g? `f $ g x`

### GHCi

- `:l` para cargar archivos
- `:r` para recargarlos (no se hace solo para los cambios)

### Layout rule

Cada definición en el mismo nivel tiene que arrancar en la misma columna
tabulada. Permite determinar la agrupación de definiciones por su indentación

```haskell
a = b + c
    where
      b = 1
      c = 2
d = a * 2
```

### Comentarios

- Ordinary: Arrancan con `--` y van hasta el final de la línea
- Nested: `{- ... -}`

## 3 - Types and classes

Un *tipo* es una colección de valores relacionados. Por ejemplo `Bool` contiene
los valores `True` y `False`, `Bool -> Bool` tiene todas las funciones que
mapean de bool en bool, como `not`. Usamos la not `v :: T` para decir que v
es un valor en el tipo T, y decimos que v tiene tipo T.

En haskell todas las expresiones deben tener un tipo. Se calcula con inferencia
de tipos. Como precede a la evaluación, los programas son *type safe* (no pueden
ocurrir errores de tipos en runtime)

> En GHCi se puede ver el tipo de una expresión con `:t` o `:type`.

### Tipos

- `Bool`
- `Char`: unicode. Single quotes
- `String`: Double quotes
- `Int`: Fixed precision
- `Integer`: Arbitrary precision (no hay límites inferiores ni superiores, toman
  tanta memoria como requieren)
- `Float`: Single precision
- `Double`: Double precision. Cantidad de memoria doble
- `List`. pueden ser infinitas
- `Tuples`: Es una secuencia finita de componentes. Escrita `(T1, T2, ..., Tn)`
  - La cantidad de elementos es la aridad

### Tipos de funciones

`T1 -> T2`. Como no hay restricciones, alcanza con 1 argumento y 1 resultado
para múltiples argumentos y resultados, por las tuplas.

> Es convención en haskell preceder a una func por su tipo, que sirve como
> documentación. El compilador chequea que el tipo dado sea consistente con el
> inferido.
>
> ```haskell
> add : (Int, Int) -> Int
> add (x, y) = x + y
> ```

No hace falta que sean totales, se pueden indefinir en ciertos inputs (ej `head`
tira una excepción con listas vacías)

### Funciones currificadas

Las funciones con más de un argumento se pueden manejar de otra forma

```haskell
add' :: Int -> (Int -> Int)
add' x y = x + y
```

Toma un int y devuelve una función que dado un int retorna un int. Es como que
hardcodea el primer argumento

El `add` de antes toma ambos argumentos de una, mientras que `add'` los toma de
a uno

Las funciones como `add'` que toman sus argumentos de a uno se llaman
**currificadas**. Son más flexibles que funciones con tuplas porque permiten
aplicación parcial. Ej. `inc = add' 1`

Para evitar el exceso de paréntesis se usan dos conv

- `->` asocia a derecha

  `Int -> Int -> Int -> Int`
  
  es

  `Int -> (Int -> (Int -> Int))

- La aplicación de funciones asocia a izquierda


  `mult x y z`

  es

  `((mult x) y) z`

### Tipos polimórficos

`length` calcula la longitud de listas de cualquier tipo.

`length :: Foldable t => t a -> Int`

Pero no solo listas, cualquier foldable. Pero se podría definir como `length ::
[a] -> Int`.

`a` es un tipo polimórfico y `length` es una func polimórfica. Hay muchas que
son así en el standard prelude

```haskell
fst :: (a,b) -> a
head :: [a] -> a
take :: Int -> [a] -> [a]
zip :: [a] -> [b] -> [(a,b)]
id :: a -> a
```

### Class constraints

Por ej. `+` se puede aplicar a números

```haskell
(+) :: Num a => a -> a -> a
```

Para todo tipo `a` *instancia* de la clase `Num` de tipos numéricos, la función
`(+)` tiene tipo `a -> a -> a`.

Una función está **overloaded** si tiene uno o más class constraints. Y estas
funciones son **métodos** de esas clases.

Se pueden ver los métodos de un typeclass con `:info`

Clases:

- `Eq` equality
- `Ord`: Equality y además con orden total
- `Show`: Se pueden convertir a strings
- `Read`: Inversa de show, leidos de strings

  Para `read "False"` no puede inferir el tipo, hay que decirle el de la exp
  final con `read "False" :: Bool`. No es el caso con `not (read "False)` en
  donde ya puede inferir que es `Bool`.
- `Num`: Numéricos
- `Integral`
- `Fractional`

## 4 - Defining functions

Condicionales y guardas, pattern matching, lambda expressions y operator
sections

Condicionales: El `if then else` de toda la vida, con else obligatorio

### Guardas

Guarded equations: secuencia de expresiones lógicas (guardas) para elegir de una
secuencia de resultados del mismo tipo.

```haskell
abs' n
    | n >= 0 = n
    | otherwise = -n
```

| se lee *such that*

Ventaja: Legibilidad,

```haskell
signum n | n < 0 = -1
         | n == 0 = 0
         | otherwise = 1

-- vs
signum n =
    if n < 0 then -1
    else if n == 0
         then 0
         else 1
```

### Pattern matching

```haskell
not :: Bool -> Bool
not False = True
not True = False
```

```haskell
(&&) :: Bool -> Bool -> Bool
True && True = True
True && False = False
False && True = False
False && False = False
```

pero se puede simplificar usando un *wildcard pattern* `_`

```haskell
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False
```

#### Tuple patterns

Una tupla de patrones es un patrón, que matchea con cualquier tupla de la misma
aridad cuyos componentes coincidan con los patrones correspondientes en orden.

```haskell
fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y
```

#### List patterns

Análogamente una lista de patrones es un patrón que matchea las listas de la
misma longitud

```haskell
test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False
```

Las listas en realidad no son primitivas, se construyen de a una a partir de []
usando `:` (cons de *cons*truct) que *prepends* un nuevo elemento al principio
de una lista existente.

`[1, 2, 3] = 1 : (2 : (3 : []))`

es una abreviación

también se puede usar para construir patrones

```haskell
test :: [Char] -> Bool
test ('a':_) = True
test _ = False
```

```haskell
head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs
```

> Tiene que tener paréntesis porque la aplicación de funciones tiene mayor
> prioridad que otros operadores. Entonces `head x:_` se interpreta como `(head
> x): _` que no es lo deseado.

### Lambda expressions

Son *nameless functions*, como `\x -> x + x`.

Se pueden usar para formalizar el significado de funciones currificadas. Por ej.

```haskell
add : Int -> Int -> Int
add x y = x + y

-- se puede entender como
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)
```

dos ventajas de escribirla así

- Queda más claro que add es una función que toma x, devuelve otra func que toma
  y y devuelve la suma
- Tienen la misma forma sintáctica `? -> (? -> ?)`

A veces permite escribir una función de forma más clara, por ej. `const` que
toma un valor y devuelve una función que siempre retorna ese valor se puede
tomar escribir de dos maneras

```haskell
const :: a -> b -> a
const x _ = x

const :: a -> (b -> a)
const x = \_ -> x
```

### Operadores

Las funciones como `+` que se escriben entre dos argumentos se llaman
*operadores*

> yo: infijos

cualquier func con dos argumentos se puede convertir en un operador usando
backticks, como

```haskell
7 `div` 2
```

también se puede la inversa. Cada operador se puede convertir en una función
currificada que se escribe antes que sus argumentos encerrándola entre
paréntesis, como

```haskell
(+) 1 2
```

Y también permite que *uno* de los argumentos se incluya en los paréntesis si se
quieren como `(1+) 2` o `(+2) 1`

En general, si `#` es un operador entonces las expresiones de la siguiente forma
se llaman **secciones**, y se pueden formalizar con exp lambda

```haskell
(#) = \x y -> x # y
(x #) = \y -> x # y
(# y) = \x -> x # y
```

Usos:

1. Construir funciones utiles de una forma compacta, como

  - `(+)` suma
  - `(1+)` sucesor
  - `(1/)` inversa
  - `(*2)` doble
  - `(/2)` mitad

2. Son necesarias para explicitar el tipo de un operador, porque los operadores
   por si solos no son expresiones válidas en haskell

   ```haskell
   (+) :: Int -> Int -> Int
   ```
  
3. Para usar operadores como argumentos a otras funciones.

  ```haskell
  sum :: [Int] -> Int
  sum = foldl (+) 0
  ```

## 5 - List comprehensions

Forma de definir funciones y listas de forma simple.

En matemáticas se pueden definir conjuntos por *comprensión*, por ej. $\{x^2
\mid x \in \{ 1..5\} \}$ es un conjunto, que se puede describir en Haskell como
`[x ^ 2 | x <- [1..5]]`

> Esto viene del *axiom of comprehension* de set theory, que hace precisa la
> idea de construir conjuntos seleccionando todos los valores que satisfacen
> cierta propiedad.

en donde `|` se lee *such that* y `<-` *drawn from* y la expresión `x <- [1..5]`
es un *generator*. Una list comprehension puede tener más de un generador,
separados por comas. Por ej. el prod cartesiano entre dos listas,

```haskell
[(x, y) | x <- [1, 2, 3], y <- [4, 5]]
```

Aspectos interesantes

- **Orden**

  Son como dos for anidados, cambiar el orden cambia el orden en el que aparecen
  las tuplas. Los futuros pueden depender de los previos

  ```haskell
  [(x, y) | x <- [1..3], y <- [x..3]]
  ```

  Otro ejemplo práctico, una func `concat` que hace flatten de una lista de
  listas
  
  ```haskell
  concat :: [[a]] -> [a]
  concat xss = [x | xs <- xss, x <- xs]
  ```

- **Wildcard**

  ```haskell
  firsts :: [(a, b)] -> [a]
  firsts ps = [x | (x, _) <- ps]
  ```

  forma falopa de definir length

  ```haskell
  length :: [a] -> Int
  length xs = sum [ 1 | _ <- xs]
  ```

- **Guardas**

  Pueden usar guardas, ej.

  ```haskell
  [x | x <- [1..10], even x]
  ```

### `zip`

La func `zip` produce una nueva lista con pares de elementos sucesivos de dos
listas hasta que una o ambas son completadas.

```haskell
> zip [’a’,’b’,’c’] [1,2,3,4]
[(’a’,1),(’b’,2),(’c’,3)]
```

### String comprehensions

Los strings en realidad son `[Char]`, por lo que cualquier func polimórfica en
listas se puede usar en strings

## 6 - Recursive functions

Da ejemplos de cómo definir funciones recursivas, que yo ya lo se

- Se puede hacer recursión en más de un argumento, como en `zip` (dos listas) o
  `drop` (lista y num)
- **Recursión multiple**: se aplica más de una vez una func en su def, como con fibonacci.
- **Recursión mútua**: Dos funciones se definen recursivamente en términos de la
  otra. Por ej. `even` y `odd` se podrían definir como

  ```haskell
  even :: Int -> Bool
  even 0 = True
  even n = odd (n-1)

  odd :: Int -> Bool
  odd 0 = False
  odd n = even (n-1)

Tips para definir funciones recursivas

1. Definir el tipo
2. Enumerar los casos (en general son standard para cada tipo)
3. Definir los casos simples (generalmente los base)
4. Definir el resto de los casos
5. Generalizar (el tipo por ej) y simplificar

Ejemplo:

```haskell
product :: [Int] -> Int
product [] = 1
product (n:ns) = x * product ns
```

es lo mismo que el esquema recursivo `foldr`, se puede escribir como

```haskell
product :: Num a => [a] -> Int
product = foldr (*) 1
```

Intento de escribir foldr

```haskell
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z [] = z
foldr' f z (x:xs) = f x (foldr f z xs)
```

## 7 - Higher order functions

Permiten que los patrones comunes de programación se encapsulen como funciones (`map`, `filter`, `foldr`)

Una *higher-order function* toma funciones como argumentos o retorna funciones como resultados. Pero como se suele decir que una func que retorna funciones está *currificada*, le decimos higher order si toma como param.

> Reflexión mía: me gusta más como en PLP se definen los esquemas recursivos como no más que una generalización de patrones comunes. En cambio acá te galerea map.

### Listas

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- def alternativa
map f xs = [f x | x <- xs]

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x then x: filter p xs else filter p xs

filter p (x:xs) = (if p x then [x] else []) ++ filter p xs

filter p (x:xs) | p x = x: filter p xs
                | otherwise = filter p xs

filter p xs = [x | x <- xs, p x]
```

Ejemplo de uso conjunto, sumar los cuadrados de los enteros pares de una lista

```haskell
sumSquares :: [Int] -> Int
sumSquares xs = sum (map (^2) (filter even xs))
```

Otras del prelude

- `all`: Todos satisfacen
- `any`: Alguno satisface
- `takeWhile`: Seleccionar elementos de una lista mientras se satisface un predicado
- `dropWhile`: Remover elementos de una lista mientras se satisfaga un predicado

### `foldr` (*fold right*)

Patrón de recursión, donde `#` es un operador que hace algo con la cabeza y el
tail de la lista

```haskell
f [] = v
f (x:xs) = x # f xs
```

por ej.

```haskell
sum = foldr (+) 0
product = foldr (*) 1
or = foldr (||) False
and = foldr (&&) True
```

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] = v
foldr f v (x:xs) = f x (foldr f v xs)
```

Otra forma de pensarlo: reemplazar cada cons por f, y el [] del final por v

```haskell
foldr (+) 0
-- a
1 : (2 : (3 : []))
1 + (2 + (3 + v))
```

Se llama *fold right* porque asume que el operador `#` asocia a derecha, porque
así lo aplica.

```haskell
foldr (#) v [x0, x1, ..., xn] = x0 # (x1 # (x2 # ... (xn # v) ... ))
```

### `foldl` (*fold left*)

Es posible que querramos un operador que asocie a izquierda, como

```haskell
sum :: Num a => [a] -> a
sum = sum' 0
  where
    sum' v [] = v
    sum' v (x:xs) = sum' (v+x) xs

sum' 0 1: (2 : (3 : [])) = (((0 + 1) + 2) + 3)

sum' (1: (2 : (3 : [])))
  = sum' (0 + 1) (2 : (3 : []))
  = sum' ((0 + 1) + 2) (3 : [])
  = sum' (((0 + 1) + 2) + 3) []
  = (((0 + 1) + 2) + 3)
```

se puede generalizar como

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ v [] = v
foldl f v (x:xs) = foldl (f v x) xs
```

```haskell
reverse :: [a] -> [a]
reverse = foldl (\xs x -> x:xs) []
reverse = foldl (flip (:)) []

reverse [1, 2, 3] = reverse (1: (2: (3: [])))
  = (3:(2:(1:[])))
```

es mejor pensar en el comportamiento de foldl como un operador que asocia a der

```haskell
foldl (#) v [x0, x1, ..., xn] = (... (((v # x0) # x1) # x2) ...) # xn
```

### Composition operator `(.)`

$f \circ g = f (g(x))$

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

-- otra peor
(f . g) x = f (g x)
```

puede servir para simplificar aplicaciones de funciones anidadas

```haskell
odd n = not (even n)
odd = not.even

twice f x = f (f x)
twice f = f.f

sumsqreven ns = sum ( map (^2) (filter even ns))
sumsqreven = sum . map (^2) . filter even
```

la 3ra usa que la composición es asociativa, `f . (g . h) = (f . g) . h`

`id` es la identidad de la composición, que permite usarla como caso base para
secencias. Por ej.

```haskell
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id
```

### `case`

Se puede usar para evitar definir funciones adicionales solo para hacer pattern matching.

## 8 - Declaring types and classes

### Type declarations

`type` introduce un **nuevo nombre** a un **tipo existente**.

```haskell
type String = [Char]
```

No pueden ser recursivas

```haskell
type Tree = (Int, [Tree])
```

Los tipos recursivos se introducen con `data`

Pueden tener tipos parametrizados

```haskell
-- Para definir funciones que manipulan pares de valores
type Pair a = (a, a)

-- Tipo de tablas de lookup que asocian claves a valores
type Assoc k v = [(k, v)]
```

### Data declarations

Para definir un nuevo tipo en lugar de un sinónimo se usa `data`.

```data
move = North | South | East | West
```

`|` se lee *or*. y cada valor nuevo del tipo se llama *constructor*.

Los nombres por si solos no tienen significado, eso lo introduce el programador
con las funciones que definen sobre esos tipos.

Pueden tener argumentos

```haskell
data Shape = Circle Float | Rect Float Float
```

`Circle` y `Rect` son *funciones constructoras*

```haskell
> :type Circle
Circle :: Float -> Shape
> :type Rect
Rect :: Float -> Float -> Shape
```

También se pueden parametrizar

```haskell
data Maybe a = Nothing | Just a
```

El libro da ejemplos de tipos recursivos (8.4) de

- `Nat` con `Succ`
- `List` igual que standard pero de 0, con `Nil` y `Cons`
- `Tree` binarios

### `newtype`

Si un nuevo tipo tiene un solo constructor con un solo argumento se puede
declarar usando `newtype`

```haskell
newtype Nat = N Int

-- vs
type Nat = Int
data Nat = N Int
```

no son sinónimos, entonces no se puede usar uno en lugar del otro
accidentalmente. Y tiene mejora de performance porque el compilador los inlinea.

### Classes y instance declarations

Mecanismo `class`

```haskell
class Eq a where
  (==), (/=) :: a -> a -> Bool

  -- definición default, que se puede pisar
  x /= y = not (x == y)

-- para que un tipo implemente Eq tiene que implementar == explícitamente (no hay duck typing)
instance Eq Bool where
    False == False = True
    True == True = True
    _ == _ = False

-- extensiones de clases
class Eq a => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool
  min, max             :: a -> a -> a

  min x y | x <= y = x
          | otherwise = y

  max x y | x <= y = y
          | otherwise = x
```

Solo tipos declarados con `data` o `newtype` pueden ser instancias de clases.

### Instancias derivadas (`deriving`)

Haskell provee facilidades para hacer que nuevos tipos sean instancias de `Eq`,
`Ord`, `Show`, `Read` automáticamente.

> Para ord, el orden de los constructores es el de su declaración

### Ejercicios

Son sencillos, parecidos a los de PLP, mejor hacer las guias.

## 9 - The countdown problem

Salteado, no parece tan interesante.