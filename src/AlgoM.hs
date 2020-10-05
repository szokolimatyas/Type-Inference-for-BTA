module AlgoM(mtest,algoM) where

import Common
import qualified Data.Map as Map
import Data.Map(Map(..))
import qualified Data.Set as Set
import Data.Set(Set(..))
import Data.Maybe
import Control.Monad.Except
import Control.Monad.State


algoM :: Expr -> TypeEnv -> Type -> [Occurence] -> TI Subst
algoM (EVar n) (TypeEnv env) p os = 
    case Map.lookup n env of
        Nothing -> error $ "undefined variable: " ++ n ++ "\n"  
        Just (TBase _)  -> mgu p (TBase os)
        Just t  -> mgu p t
algoM (EVar_ n) _  p _ = mgu p TUntyped

algoM (EApp e1 e2) env p os = do
    b <- freshTyVar
    s1 <- algoM e1 env (TArrow b p os) $ OFun : os
    s2 <- algoM e2 (apply s1 env) (apply s1 b) $ OArg : os
    return (s2 `composeSubst` s1)
algoM (EApp_ e1 e2) tenv p os = do
    s1 <- mgu p TUntyped
    s2 <- algoM e1 (apply s1 tenv) TUntyped $ OFun : os  
    s3 <- algoM e2 (apply (s2 `composeSubst` s1) tenv) TUntyped $ OArg : os
    return (s3 `composeSubst` s2 `composeSubst` s1)

algoM (EAbs n e) tenv p os = do
    b1 <- freshTyVar
    b2 <- freshTyVar
    s1 <- mgu p (TArrow b1 b2 os)
    let TypeEnv env' = apply s1 $ remove tenv n
    s2 <- algoM e (TypeEnv $ env' `Map.union` Map.singleton n (apply s1 b1)) (apply s1 b2) (OBody:os) 
    return (s2 `composeSubst` s1)
algoM (EAbs_ n e) tenv  p os = do
    s1 <- mgu p TUntyped  
    let TypeEnv env' = apply s1 $ remove tenv n
    s2 <- algoM e (TypeEnv $ env' `Map.union` Map.singleton n TUntyped) TUntyped (OBody:os)
    return (s2 `composeSubst` s1)

algoM EConst env p os = mgu p (TBase os)
algoM EConst_ env p os = mgu p TUntyped

algoM (EIf cond ex1 ex2) env p os = do
    s1 <- algoM cond env (TBase os)  $ OCond : os
    s2 <- algoM ex1 (apply s1 env) (apply s1 p)  $ OThen : os
    s3 <- algoM ex2 (apply (s2 `composeSubst` s1) env) (apply (s2 `composeSubst` s1) p) $ OElse : os
    return (s3 `composeSubst` s2 `composeSubst` s1)
algoM (EIf_ cond ex1 ex2) env p os = do
    s1 <- mgu p TUntyped
    s2 <- algoM cond (apply s1 env) TUntyped  $ OCond : os 
    s3 <- algoM ex1 (apply (s2 `composeSubst` s1) env) TUntyped  $ OThen : os
    s4 <- algoM ex2 (apply (s3 `composeSubst` s2 `composeSubst` s1) env) TUntyped $ OElse : os
    return (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1)

typeInference' :: TypeEnv -> Expr -> TI Type
typeInference' (TypeEnv env) e = do
    t <- freshTyVar 
    s <- algoM e (TypeEnv env) t []
    return (apply s t)

mtest :: TypeEnv -> Expr -> Expr 
mtest m e = 
    let (res, _) = evalTI (typeInference' m e) in
    case res of
        Left err ->
            mtest m $ underline e (reverse err)
        Right t  -> e
                