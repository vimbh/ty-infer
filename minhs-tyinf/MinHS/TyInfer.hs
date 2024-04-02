module MinHS.TyInfer where
import Debug.Trace

import MinHS.Bwd
import MinHS.Syntax
    ( BaseType(Unit, Int, Bool),
      Type(..),
      Scheme(..),
      Op(Snd, Gt, Ge, Lt, Le, Eq, Ne, Neg, Fst),
      SBind(..),
      MBind(MBind),
      Alt(Alt),
      Exp(..),
      Program,
      Id,
      frv,
      ffv,
      fix )
import qualified MinHS.Subst as S (Subst, SubstSem, substFlex, substRigid, fromList)
import MinHS.TCMonad

import Control.Monad (foldM)
import Data.List (delete)
import Data.Set (Set, notMember, fromList, difference, member, toList, union, empty)
import Data.Bifunctor (Bifunctor(..), second)
import MinHS.Subst ((=:))

-- | A monomorphic type injected into a type scheme with no quantified
-- type variables.
mono :: Type -> Scheme
mono t = Forall [] t

primOpType :: Op -> Scheme
primOpType Gt   = mono $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ge   = mono $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Lt   = mono $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Le   = mono $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Eq   = mono $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ne   = mono $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Neg  = mono $ Base Int `Arrow` Base Int
primOpType Fst  = Forall ["a","b"] $ (RigidVar "a" `Prod` RigidVar "b") `Arrow` RigidVar "a"
primOpType Snd  = Forall ["a","b"] $ (RigidVar "a" `Prod` RigidVar "b") `Arrow` RigidVar "b"
primOpType _    = mono $ Base Int `Arrow` (Base Int `Arrow` Base Int)

conType :: Id -> Maybe Scheme
conType "True"  = Just $ mono $ Base Bool
conType "False" = Just $ mono $ Base Bool
conType "()"    = Just $ mono $ Base Unit
conType "Pair"  = Just
                  $ Forall ["a","b"]
                  $ RigidVar "a" `Arrow` (RigidVar "b" `Arrow` (RigidVar "a" `Prod` RigidVar "b"))
conType "Inl"   = Just
                  $ Forall ["a","b"]
                  $ RigidVar "a" `Arrow` (RigidVar "a" `Sum` RigidVar "b")
conType "Inr"   = Just
                  $ Forall ["a","b"]
                  $ RigidVar "b" `Arrow` (RigidVar "a" `Sum` RigidVar "b")
conType _       = Nothing

freshForall :: [Id] -> TC [(Id,Id)]
freshForall xs = mapM (\x -> (,) <$> pure x <*> freshId) xs

-- | Produce fresh flexible variables for a type scheme
specialise :: Scheme -> TC (Type, Suffix)
specialise (Forall xs t) =
  do ids <- freshForall xs
     return (S.substRigid (S.fromList (map (second FlexVar) ids)) t
            , map (flip (,) HOLE . snd) ids)

-- | Extend a typing context with a collection of flexible declarations
extend :: Gamma -> Suffix -> Gamma
extend g [] = g
extend g ((v,d) : ds) = extend (g :< v := d) ds


infer :: Program -> Either TypeError (Program, Gamma)
infer program = runTC $ inferProgram BEmpty program

-- | Perform unification of the given types
unify :: Gamma -> Type -> Type -> TC Gamma

unify g (Base b1) (Base b2) 
  | b1 == b2 = return g
  | otherwise = typeError $ TypeMismatch (Base b1) (Base b2) (error "Type Mismatch")

unify g (FlexVar id) t = inst g [] id t
unify g t (FlexVar id) = inst g [] id t

unify g (Arrow t1 t2) (Arrow t1' t2') = do
  g' <- unify g t1 t1'
  unify g' t2 t2'

unify g (Prod t1 t2) (Prod t1' t2') = do
  g' <- unify g t1 t1'
  unify g' t2 t2'

unify g (Sum t1 t2) (Sum t1' t2') = do
  g' <- unify g t1 t1'
  unify g' t2 t2'

unify g (RigidVar id) (RigidVar id')
  | id == id' = return g
  | otherwise = typeError $ TypeMismatch (RigidVar id) (RigidVar id') (error "Type Mismatch")

unify g t1 t2 = typeError $ UnifyFailed g t1 t2

-- | Instantiation the type variable a with the type t
inst :: Gamma -> Suffix -> Id -> Type -> TC Gamma
inst g suffix a t = case g of
    BEmpty -> return BEmpty
    gamma :< entry -> do
        gamma' <- inst gamma suffix a t
        case entry of
            var := Defn typeVar -> do
                let typeVar' = if var == a then t else typeVar
                return (gamma' :< (var := Defn typeVar'))
            _ -> return (gamma' :< entry)



-----------------------------------------------------------------------------------------------


-- Run and return inferred program
inferProgram :: Gamma -> Program -> TC (Program, Gamma)
inferProgram g (SBind x _ e) = do
  (t, g1) <- inferExp g e
  let scheme = generalise g1 t
  return (SBind x (Just scheme) e, g1)
-----------------------------------------------------------------------------------------------
-- Infer expressions begin here

inferExp :: Gamma -> Exp -> TC (Type, Gamma)

-- Base types
inferExp g (Num _) = return (Base Int, g)
inferExp g (Con "True") = return (Base Bool, g)
inferExp g (Con "False") = return (Base Bool, g)

-- Primops
inferExp g (App (App (Prim op) e1) e2) = do
  opTypeScheme <- specialise (primOpType op) -- Specialise any rigid vars to flex vars
  case opTypeScheme of
    (Arrow t1 (Arrow t2 t3), _) -> do   -- type -> type -> type
      (t1', g1) <- inferExp g e1        -- t1': type of e1
      g2 <- unify g1 t1 t1'             -- check type e1 matches with type t1 exp by App
      (t2', g3) <- inferExp g2 e2       -- t2': type of e2
      g4 <- unify g3 t2 t2'             -- check type e2 matches with type t2 exp by App
      return (t3, g4)
    _ -> typeError $ FunctionTypeExpected (App (Prim op) e1) (fst opTypeScheme)

-- Primops : urnary negate
inferExp g (App (Prim Neg) e1) = do
  let opTypeScheme = primOpType Neg  
  (opType, _) <- specialise opTypeScheme
  case opType of
    Arrow t1 t2 -> do
      (t1', g1) <- inferExp g e1
      g2 <- unify g1 t1 t1'
      return (t2, g2)
    _ -> typeError $ FunctionTypeExpected (Prim Neg) opType

-- Let expressions
inferExp g (Let binds e) = do
  (_, g') <- foldM processBind ([], g) binds
  (t, g'') <- inferExp g' e

  return (t, g'')

-- Var eval
inferExp g (Var x) = do

  case lookupVar g x of
    Just scheme -> do
      (t, _) <- specialise scheme
      return (t, g)
    Nothing -> error $ "var " ++ x ++ "is not bound"

-- If then else
inferExp g (If eCond eThen eElse) = do

  (tCond, g1) <- inferExp g eCond
  g2 <- unify g1 tCond (Base Bool)

  (tThen, g3) <- inferExp g2 eThen
  (tElse, g4) <- inferExp g3 eElse

  g5 <- unify g4 tThen tElse

  return (tThen, g5)

-- Products
inferExp g (Con "()") = return (Base Unit, g)

-- Pair
inferExp g (App (App (Con "Pair") e1) e2) = do
  (t1, g1) <- inferExp g e1
  (t2, g2) <- inferExp g1 e2
  return (Prod t1 t2, g2)

-- Pair First
inferExp g (App (Prim Fst) e) = do
  (t, g1) <- inferExp g e
  case t of
    Prod t1 _ -> return (t1, g1)
    _ -> error "fst applied to non-product type"

-- Pair Second
inferExp g (App (Prim Snd) e) = do
  (t, g1) <- inferExp g e
  case t of
    Prod _ t2 -> return (t2, g1)
    _ -> error "snd applied to non-product type"

-- Sums

-- InLeft
inferExp g (App (Con "Inl") e) = do
    (t, g1) <- inferExp g e
    freshVar <- freshId  
    let sumType = Sum t (FlexVar freshVar)
    let g2 = extend g1 [(freshVar, Defn (FlexVar freshVar))]

    return (sumType, g2)

-- InRight
inferExp g (App (Con "Inr") e) = do
  (t, g1) <- inferExp g e
  freshVar <- freshId
  let sumType = Sum (FlexVar freshVar) t
  let g2 = extend g1 [(freshVar, Defn (FlexVar freshVar))] -- Adjust if necessary
  return (sumType, g2)
  
-- Case Expressions
inferExp g (Case e alts) = case alts of
    [Alt "Inl" [x] e1, Alt "Inr" [y] e2] -> do
        (t, g') <- inferExp g e
        case t of
            Sum tLeft tRight -> do
                -- InLeft
                let gInl = extend g' [(x, Defn tLeft)]
                (tInl, _) <- inferExp gInl e1

                -- InRight
                let gInr = extend g' [(y, Defn tRight)]
                (_, gInr') <- inferExp gInr e2

                return (tInl, gInr')

            _ -> typeError $ TypeMismatch t (Sum (Base Unit) (Base Unit)) e
    _ -> typeError MalformedAlternatives


-- Function Application (non prim-op)
inferExp g (App e1 e2) = do

  (t1, g1) <- inferExp g e1
  (t2, g2) <- inferExp g1 e2
  freshVar <- freshId
  let resultType = FlexVar freshVar

  g3 <- unify g2 t1 (Arrow t2 resultType)
  return (resultType, g3)


-- RecFun
inferExp g (Recfun (MBind f x e)) = do

    freshParamVar <- freshId
    freshResultVar <- freshId
    let paramType = FlexVar freshParamVar
    let resultType = FlexVar freshResultVar

    let funcType = Arrow paramType resultType
    let extendedContext = extend g [(f, Defn funcType), (x, Defn paramType)]

    (bodyType, g1) <- inferExp extendedContext e
    g2 <- unify g1 resultType bodyType
    g3 <- unify g2 paramType (Base Int)
    g4 <- unify g3 resultType (Base Int)

    return (funcType, g4)

-- Uncaught inferExp case??
_ = 
    -- traceShow ("inferExp called with expression: ", e, "in context: ", g)  $
    error "Uncaught inferExp"


-----------------------------------------------------------------------------------------------
-- Helper Funs

-- Generalise: create type scheme by quantifying flex vars
generalise :: Gamma -> Type -> Scheme
generalise g t = 
    let rigidFreeVars = frv t `difference` frvGamma g
        flexibleFreeVars = ffv t
        allFreeVars = rigidFreeVars `union` flexibleFreeVars
    in Forall (toList allFreeVars) (fix (toList flexibleFreeVars) t)

-- Get all type variables that are already bound
frvGamma :: Gamma -> Set Id
frvGamma BEmpty = empty
frvGamma (gamma :< (_ := Defn t)) = frv t `union` frvGamma gamma
frvGamma (gamma :< _) = frvGamma gamma

-- Create and map fresh vars for a scheme
instantiateScheme :: Scheme -> TC Type
instantiateScheme (Forall vars t) = do
  freshVars <- freshForall vars
  let substitutions = map (\(old, new) -> (old, FlexVar new)) freshVars
  return $ applySubstitutions substitutions t

-- Subsitute fresh vars into a scheme
applySubstitutions :: [(Id, Type)] -> Type -> Type
applySubstitutions subs t = foldl (\acc (var, t') -> S.substFlex (var =: t') acc) t subs
  
-- Lookup var in context and find its type scheme
lookupVar :: Gamma -> Id -> Maybe Scheme
lookupVar BEmpty _ = Nothing
lookupVar (gamma :< (varName := Defn t)) x 
  | x == varName = Just (mono t)  
  | otherwise    = lookupVar gamma x
lookupVar (gamma :< _) x = lookupVar gamma x

-- Infer a bindings type and update it with the inf. type scheme
processBind :: ([SBind], Gamma) -> SBind -> TC ([SBind], Gamma)
processBind (acc, g) (SBind x _ e1) = do
  (t1, g1) <- inferExp g e1
  let scheme1 = generalise g1 t1
  let updatedSBind = SBind x (Just scheme1) e1
  -- traceM $ "Processing SBind for variable: " ++ x ++ ", inferred type: " ++ show scheme1
  let g2 = extend g1 [(x, Defn t1)]

  return (updatedSBind : acc, g2)



