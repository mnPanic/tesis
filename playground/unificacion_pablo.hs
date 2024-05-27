type Metavar = Int

data Term
    = TMetavar Metavar
    | TVar String
    | TFun Term Term

type Substitution = Map Metavar Term

representative :: Substitution -> Term -> Term
representative subst (TMetavar x) =
    case Map.lookup x subst of
        Nothing -> TMetavar x
        Just t -> representative subst t
representative subst t = t

occurs :: Substitution -> Metavar -> Term -> Bool
occurs subst x t0 =
    let t = representative subst t0
     in case t of
            TMetavar y -> x == y
            TVar _ -> False
            TFun t1 t2 -> occurs subst x t1 || occurs subst x t2

unify :: Substitution -> Term -> Term -> Either ErrMsg Substitution
unify subst t0 s0 =
    let t = representative subst t0
        s = representative subst s0
     in case (t, s) of
            (TMetavar x, TMetavar y) | x == y -> Right subst
            (TMetavar x, t) ->
                if occurs subst x t
                    then Left "Occurs check"
                    else Right (Map.insert x t subst)
            (t, TMetavar x) ->
                if occurs subst x t
                    then Left "Occurs check"
                    else Right (Map.insert x t subst)
            (TVar s1, TVar s2) | s1 == s2 -> Right subst
            (TFun t1 t2, TFun s1 s2) ->
                case unify subst t1 s1 of
                    Left errmsg -> Left errmsg
                    Right subst' -> unify subst' t2 s2
            _ -> Left "No unifican las cosas"