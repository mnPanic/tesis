data Maybe' a = Just' a | Nothing' deriving (Show)

instance Functor Maybe' where
    -- fmap :: (a -> b) -> Maybe' a -> Maybe' b
    fmap f (Just' x) = Just' (f x)
    fmap _ Nothing' = Nothing'

instance Applicative Maybe' where
    -- pure :: a -> Maybe' a
    pure = Just'

    -- <*> :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
    (<*>) Nothing' _ = Nothing'
    (<*>) (Just' f) mx = fmap f mx

{--

ghci > pure (+ 1) <*> (Just' 1)
Just' 2
ghci > pure (+) <*> (Just' 1) <*> (Just' 2)
Just' 3
ghci > pure (\x y z -> x + y * z) <*> (Just' 1) <*> (Just' 2) <*> Nothing'
Nothing'
ghci > pure (\x y z -> x + y * z) <*> (Just' 1) <*> (Just' 2) <*> (Just' 3)
Just' 7

Ejecuta una funcion que puede fallar a argumentos que pueden fallar, dando como
resultado algo que puede fallar (Maybe).

Maybe de esta forma permite Exceptional programming en el cual podemos aplicar
funciones puras a argumentos que pueden fallar sin la necesidad de manejar la
propagación de errores nosotros, ya que se encarga la maquinaria del aplicative.
--}

-- Monadas

data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
    Nothing -> Nothing
    Just n -> case eval y of
        Nothing -> Nothing
        Just m -> safediv n m

-- y `safediv` no encaja en el patrón de applicative functors, entonces no se
-- puede escribir
-- eval (Div x y) = pure safediv <*> eval x <*> eval y
-- porque safediv no es Int -> Int -> Int sino Int -> Int -> Maybe Int

{-- Podemos abstraer el patron de evaluar algo, si es nothing devolver nothing y
  sino devolver eso con el siguiente operador

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
            Nothing -> Nothing
            Just x -> f x
--}

eval' :: Expr -> Maybe Int
eval' (Val n) = Just n
eval' (Div x y) =
    eval x >>= \n ->
        eval y >>= \m ->
            safediv n m

{--
En general va a ser

    m1 >>= \x1 ->
    m2 >>= \x2 ->
    .
    .
    .
    mn >>= \xn ->
    f x1 x2 ... xn

pero es incómodo de escribir, entonces haskell provee una versión más fácil, el
do! que ya venia usando en el chap 10

    do x1 <- m1
       x2 <- m2
       .
       .
       .
       xn <- mn
       f x1 x2 ... xn
--}

eval'' :: Expr -> Maybe Int
eval'' (Val n) = Just n
eval'' (Div x y) = do
    n <- eval x
    m <- eval y
    safediv n m