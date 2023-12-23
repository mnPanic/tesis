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