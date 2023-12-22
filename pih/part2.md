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