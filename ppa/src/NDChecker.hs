module NDChecker (
    check,
    CheckResult (..),
    subst,
    rootCause,
    checkResultIsErr,
) where

import ND (
    Env (..),
    Form (..),
    FunId,
    PredId,
    Proof (..),
    Subst,
    Term (..),
    VarId,
    fv,
    fvE,
    fvTerm,
    get,
 )

import Data.Map qualified as Map
import Data.Set qualified as Set
import Text.Printf (printf)

-- sustituye todas las ocurrencias libres de VarId por Term.
--
-- Lo hace alpha-renombrando en el camino para evitar *capturas* de variables:
-- si y esta libre en T, busca y' que no este libre en T y f1. En lugar de
-- reemplazar en el momento, lo guarda en una Subst para hacerlo lineal y no
-- cuadrático.
subst :: VarId -> Term -> Form -> Form
subst = subst' Map.empty

subst' :: Subst -> VarId -> Term -> Form -> Form
subst' s x t f = case f of
    FPred l ts -> FPred l (map (substTerm s x t) ts)
    FAnd f1 f2 -> FAnd (rec f1) (rec f2)
    FOr f1 f2 -> FOr (rec f1) (rec f2)
    FImp f1 f2 -> FImp (rec f1) (rec f2)
    FNot f1 -> FNot (rec f1)
    FTrue -> FTrue
    FFalse -> FFalse
    orig@(FForall y f1)
        -- No está libre, no cambio
        | x == y -> orig
        -- Captura de variables
        | y `elem` fvTerm t -> FForall y' (recRenaming y y' f1)
        | otherwise -> FForall y (rec f1)
      where
        y' = freshWRT y $ fv f1 `Set.union` fvTerm t
    orig@(FExists y f1)
        -- No está libre, no cambio
        | x == y -> orig
        -- Captura de variables
        | y `elem` fvTerm t -> FExists y' (recRenaming y y' f1)
        | otherwise -> FExists y (rec f1)
      where
        y' = freshWRT y $ fv f1 `Set.union` fvTerm t
  where
    rec = subst' s x t
    recRenaming y y' = subst' (Map.insert y y' s) x t

substTerm :: Subst -> VarId -> Term -> Term -> Term
substTerm s x t t' = case t' of
    -- No es necesario chequear que el renombre de x se quiera sustituir, porque
    -- renombramos cuando encontramos un cuantificador, y si la var del
    -- cuantificador es la misma de la subst entonces no está libre y hubiera
    -- cortado (nunca llega acá)
    o@(TVar y)
        | x == y -> t
        | otherwise -> maybe o TVar (Map.lookup y s)
    TFun f ts -> TFun f (map (substTerm s x t) ts)

-- freshWRT da una variable libre con respecto a una lista en donde no queremos
-- que aparezca
freshWRT :: (Foldable t) => VarId -> t VarId -> VarId
freshWRT x forbidden = head [x ++ suffix | suffix <- map show [0 ..], x ++ suffix `notElem` forbidden]

data CheckResult
    = CheckOK
    | CheckError Env Proof Form String
    | CheckErrorW String Env Proof Form CheckResult
    | CheckErrorN
        { name :: String
        , err :: CheckResult
        }
    deriving (Eq)

instance Show CheckResult where
    show CheckOK = "OK"
    show (CheckError env proof form msg) =
        printf
            "Check error! %s on\nproof: %s\n%s\n|- %s"
            msg
            (show proof)
            (show env)
            (show form)
    show (CheckErrorW msg env proof form res) =
        printf
            "Checking \nproof: %s\n%s\n|- %s\n\n%s:\n%s"
            (show proof)
            (show env)
            (show form)
            msg
            (showInTree res)
    show (CheckErrorN name res) =
        printf
            "Checking proof '%s': \n%s"
            name
            (show res)

showInTree :: CheckResult -> String
showInTree (CheckError env proof form msg) =
    printf
        "Check error! %s on\nproof: %s\n%s\n|- %s"
        msg
        (show proof)
        (show env)
        (show form)
showInTree (CheckErrorW msg env proof form res) =
    printf
        "Checking\n%s\n|- %s\n\n%s:\n%s"
        (show env)
        (show form)
        msg
        (showInTree res)
showInTree (CheckErrorN name res) =
    printf
        "Checking proof '%s': \n%s"
        name
        (showInTree res)

checkResultIsErr :: CheckResult -> Bool
checkResultIsErr (CheckError{}) = True
checkResultIsErr (CheckErrorW{}) = True
checkResultIsErr (CheckErrorN{}) = True
checkResultIsErr (CheckOK{}) = False

-- rootCause grabs the errs root cause.
-- pre: checkResultIsErr err = True
rootCause :: CheckResult -> CheckResult
rootCause e@(CheckError{}) = e
rootCause (CheckErrorW _ _ _ _ e) = rootCause e
rootCause (CheckErrorN _ e) = rootCause e

wrap :: CheckResult -> String -> Env -> Proof -> Form -> CheckResult
wrap res msg env proof form = case res of
    err | checkResultIsErr err -> CheckErrorW msg env proof form err
    CheckOK -> CheckOK

{- TODO:
    - Mónadas para chaining de errores? Por ej en PImpE
-}
check :: Env -> Proof -> Form -> CheckResult
check env (PNamed name proof) f = case check env proof f of
    err | checkResultIsErr err -> CheckErrorN name err
    CheckOK -> CheckOK
-- TODO: handlearlo cuando es al reves, not A v A que es lo mismo
check env PLEM form@(FOr f1 (FNot f2)) =
    if f1 == f2
        then CheckOK
        else CheckError env PLEM form "LEM proves A v ~A for the same form A, but they are different"
check env proof@(PAx hyp) f =
    case get env hyp of
        Just f' ->
            if f == f'
                then CheckOK
                else CheckError env proof f (printf "env has hyp '%s' for different form '%s'" hyp (show f'))
        Nothing -> CheckError env proof f (printf "hyp %s not in env" hyp)
-- no importa quien es f, false demuestra cualquier cosa
check env (PFalseE pFalse) _ = check env pFalse FFalse
check env PTrueI FTrue = CheckOK
-- dem A -> B
check env p@(PImpI hyp proofB) f@(FImp fA fB) =
    wrap (check (EExtend hyp fA env) proofB fB) "proof consequent" env p f
-- dem B con A -> B
check env proof@(PImpE fA proofAImpB proofA) fB =
    case check env proofAImpB (FImp fA fB) of
        err | checkResultIsErr err -> CheckErrorW "proof imp" env proof fB err
        CheckOK -> wrap (check env proofA fA) "proof antecedent" env proof fB
-- dem not A
check env (PNotI hyp proofFalse) (FNot f) =
    check (EExtend hyp f env) proofFalse FFalse
-- dem False con not A
check env proof@(PNotE fA proofNotA proofA) FFalse =
    case check env proofNotA (FNot fA) of
        err | checkResultIsErr err -> CheckErrorW "proof not form" env proof FFalse err
        CheckOK -> wrap (check env proofA fA) "proof form" env proof FFalse
-- dem C con A v B
check env proof@(POrE fA fB proofAorB hypA proofAC hypB proofBC) fC =
    case check env proofAorB (FOr fA fB) of
        err | checkResultIsErr err -> CheckErrorW "proof or" env proof fC err
        CheckOK -> case check (EExtend hypA fA env) proofAC fC of
            err | checkResultIsErr err -> CheckErrorW "proof assuming left" env proof fC err
            CheckOK -> wrap (check (EExtend hypB fB env) proofBC fC) "proof assuming right" env proof fC
-- dem A v B
check env p@(POrI1 proofA) f@(FOr fA _) =
    wrap (check env proofA fA) "proof left" env p f
check env p@(POrI2 proofB) f@(FOr _ fB) =
    wrap (check env proofB fB) "proof right" env p f
-- dem con A ^ B
check env p@(PAndE1 fB proofAnB) fA =
    wrap (check env proofAnB (FAnd fA fB)) "proof and" env p fA
check env p@(PAndE2 fA proofAnB) fB =
    wrap (check env proofAnB (FAnd fA fB)) "proof and" env p fA
-- dem de A ^ B
check env proof@(PAndI proofA proofB) f@(FAnd fA fB) =
    case check env proofA fA of
        err | checkResultIsErr err -> CheckErrorW "proof left" env proof f err
        CheckOK -> wrap (check env proofB fB) "proof right" env proof f
-- dem de Exists x. A, prueba A{x := t}
check env p@(PExistsI t proofSubstA) fE@(FExists x f) =
    wrap (check env proofSubstA (subst x t f)) "proof A{x := t}" env p fE
-- del de B con Exists x. A
check env proof@(PExistsE x fA proofExistsxA hypA proofB) fB
    | x `elem` fvE env = CheckError env proof fB (printf "env shouldn't contain fv '%s'" x)
    | x `elem` fv fB = CheckError env proof fB (printf "form to prove shoudln't contain fv '%s'" x)
    | otherwise = case check env proofExistsxA (FExists x fA) of
        err | checkResultIsErr err -> CheckErrorW "proof exists" env proof fB err
        CheckOK -> wrap (check (EExtend hypA fA env) proofB fB) "proof assuming" env proof fB
-- dem de Forall x. A
check env proof@(PForallI x' proofA) form@(FForall x fA)
    | x' `elem` fvE env = CheckError env proof form (printf "env shouldn't contain fv '%s'" x')
    | x' `elem` fv form = CheckError env proof form (printf "new var (%s) shouldn't be in fv of forall form %s" x' (show form))
    | otherwise =
        let fA' = subst x (TVar x') fA
         in wrap (check env proofA fA') "proof form" env proof form
-- dem de A{x:=t} usando Forall x. A
check env proof@(PForallE x fA proofForallxA t) fAxt =
    if subst x t fA /= fAxt
        then
            CheckError
                env
                proof
                fAxt
                ( printf
                    "form %s /= (%s){%s := %s}"
                    (show fAxt)
                    (show fA)
                    x
                    (show t)
                )
        else wrap (check env proofForallxA (FForall x fA)) "proof forall" env proof fAxt
-- Error para agarrar todo lo no handleado
check env proof form = CheckError env proof form "Unhandled proof"