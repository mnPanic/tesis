# Notas de Programming in Haskell de Graham Hutton - Part 2

## 10 - Interactive programming

Se puede pensar al monad IO como `type IO a = World -> (a, World)`. Pero en la
práctica está implementado en haskell y sus detalles de implementación ocultos

```haskell
data IO a = ...
```

Acciones básicas

- `getChar`
- `putChar`
- `return`

Secuenciar

```haskell
do v1 <- a1
   a2 -- eq a _ <- a2
   .
   .
   .
   vn <- an
   return (f v1 v2 ... vn)
```

Es buena práctica en haskell tratar de dividir en cosas que necesitan IO y cosas
puras para minimizar el uso de side effects.

## 11 - Unbeatable tic tac toe

Salteado porque es muy parecido al juego min-nim del tp de teoría de juegos

https://github.com/mnPanic/tdj-tps/tree/master/tp3-min-nim

## 12 - Monads and more

Los tres patrones que se encaran en este capítulo son una forma de abstraer
patrones comunes de programación como definiciones: functores, applicatives y monads.

### Functors

Son una generalización de tipos que son mapeables con una función (en el sentido
de `map`)

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Toma una función de tipo `a -> b` y una estructura de tipo `f a` cuyos elementos
tienen tipo `a`. Aplica la función y devuelve otra estructura de tipo `f b`

> Mini rabbit hole. No todos los foldables pueden ser expresados como functores,
> son menos generales. https://stackoverflow.com/questions/8359115/an-example-of-a-foldable-which-is-not-a-functor-or-not-traversable

### Applicatives

https://stackoverflow.com/questions/3242361/what-is-called-and-what-does-it-do/3242853#3242853

fmap solo te deja aplicar una función de tipo `a -> b`, pero que pasa si
querríamos aplicar una función de cualquier aridad?

```haskell
fmap0 :: a -> f a
fmap1 :: (a -> b) -> f a -> f b
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
... etc
```

se puede hacer usando currificación y funciones

```haskell
pure :: a -> f a

(<*>) :: f (a -> b) -> f a -> f b
```

la clase de functors que soportan `pure` y `<*>` son *applicative functors* o
*applicatives*.

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

También sirven para hacer *effectful programming*

### Monads

> Video de graham hutton que usa el mismo ejemplo en computerphile
> https://www.youtube.com/watch?v=t1e8gqXLbsU

```haskell
class Applicative m => Monad f where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

  return = pure
```

a `>>=` también se lo suele llamar *bind* porque el segundo argumento bindea el
resultado del primero