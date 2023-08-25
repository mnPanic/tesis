-- Tautology checker. Se chequea si una form es tautología con su tabla de
-- verdad

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k']

data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop

-- Sustitución para saber valores de variables prop
type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var k) = find k s
eval s (Not p) = not $ eval s p
eval s (And p1 p2) = (eval s p1) && (eval s p2)
eval s (Imply p1 p2) = not (eval s p1) || (eval s p2)

isTaut :: Prop -> Bool
taut p = all id (map (flip eval p) (allSubst p))

allSubst :: Prop -> [Subst]
allSubst p = allSubst' $ vars p

allSubst' :: [Char] -> [Subst]
allSubst' [] = [[]]
allSubst' (v:vs) = [ (v, b):s | s <- allSubst' vs, b <- [True, False] ]

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var k) = [k]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2

p1 :: Prop
p1 = And (Var 'A')
         (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B'))
           (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A')
           (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A')
                (Imply (Var 'A') (Var 'B')))
           (Var 'B')

-- a v no a = no (no a y a)
pTaut = Not p1