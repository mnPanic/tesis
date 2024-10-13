-- NDChecker contiene lo necesario para poder `check`ear una ND.Proof
module NDChecker (
    check,
    CheckResult (..),
    rootCause,
    checkResultIsErr,
) where

import ND (
    Env (..),
    Form (..),
    Proof (..),
    Term (..),
    formsWithFv,
    fv,
    fvE,
    get,
    proofName,
 )

import NDSubst (subst)

import Text.Printf (printf)

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
            "Checking \nproof: %s\n%s\n|- %s\n\n(%s) %s:\n%s"
            (show proof)
            (show env)
            (show form)
            (proofName proof)
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
        "Checking\n%s\n|- %s\n\n(%s) %s:\n%s"
        (show env)
        (show form)
        (proofName proof)
        msg
        (showInTree res)
showInTree (CheckErrorN name res) =
    printf
        "Checking proof '%s': \n%s"
        name
        (showInTree res)
showInTree CheckOK = undefined

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
rootCause (CheckOK) = undefined

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
check _ PTrueI FTrue = CheckOK
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
    | x `elem` fv fB = CheckError env proof fB (printf "form to prove shouldn't contain fv '%s'" x)
    | otherwise = case check env proofExistsxA (FExists x fA) of
        err | checkResultIsErr err -> CheckErrorW "proof exists" env proof fB err
        CheckOK -> wrap (check (EExtend hypA fA env) proofB fB) "proof assuming" env proof fB
-- dem de Forall x. A
check env proof@(PForallI x' proofA) form@(FForall x fA)
    | x' `elem` fvE env = CheckError env proof form (printf "env shouldn't contain fv '%s', forms: %s" x' (show $ formsWithFv env x))
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