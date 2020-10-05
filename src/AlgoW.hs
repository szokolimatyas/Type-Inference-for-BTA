module AlgoW(wtest,algoW) where

import Common
import qualified Data.Map as Map
import Data.Map(Map(..))
import qualified Data.Set as Set
import Data.Set(Set(..))
import Data.Maybe
import Control.Monad.Except
import Control.Monad.State


algoW :: Expr -> TypeEnv -> [Occurrence] -> TI (Subst,Type)
algoW (EVar n) (TypeEnv env) os =
    case Map.lookup n env of
        Nothing -> error $ "undefined variable: " ++ n ++ "\n"  
        Just (TBase _) -> return (nullSubst, TBase os) 
        Just t  -> return (nullSubst,t)
algoW (EVar_ n) tenv os = return (nullSubst,TUntyped)
algoW (EApp e1 e2) tenv os = do
    (s1,t1) <- algoW e1 tenv $ OFun : os
    (s2,t2) <- algoW e2 (apply s1 tenv) $ OArg : os
    new <- freshTyVar
    v <- mgu (apply s2 t1) (TArrow t2 new os) 
    return (v `composeSubst` s2 `composeSubst` s1, apply v new)
algoW (EApp_ e1 e2) tenv os = do
    (s1,t1) <- algoW e1 tenv $ OFun : os
    (s2,t2) <- algoW e2 (apply s1 tenv) $ OArg : os
    v <- mgu' [TUntyped, apply s2 t1, t2] 
    return (v `composeSubst` s2 `composeSubst` s1, TUntyped)
algoW (EAbs n e) env os = do
    tv <- freshTyVar
    let TypeEnv env' = remove env n
        env''        = TypeEnv $ env' `Map.union` Map.singleton n tv
    (s1,t1) <- algoW e env'' $ OBody : os
    return  (s1, TArrow (apply s1 tv) t1 os)
algoW (EAbs_ n e) env os = do
    let TypeEnv env' = remove env n
        env''        = TypeEnv $ env' `Map.union` Map.singleton n TUntyped
    (s1,t1) <- algoW e env'' $ OBody : os
    v <- mgu TUntyped t1
    return  (v `composeSubst` s1, TUntyped)
algoW EConst env os = return (nullSubst, TBase os)
algoW EConst_ env os = return (nullSubst, TUntyped)
algoW (EFix ex) env os = do
    tv <- freshTyVar
    (s1,t1) <- algoW ex env $ OFixBody : os
    v <- mgu t1 (TArrow tv tv os) 
    return (v `composeSubst` s1, apply v tv) 
algoW (EFix_ ex) env os = do
    (s1,t1) <- algoW ex env $ OFixBody : os
    v <- mgu TUntyped t1
    return (v `composeSubst` s1, TUntyped) 
algoW (EIf cond ex1 ex2) env os = do
    (s1,t1) <- algoW cond env $ OCond : os
    v1 <- mgu (TBase os) t1
    (s2,t2) <- algoW ex1 (apply (v1 `composeSubst` s1) env) $ OThen : os
    (s3,t3) <- algoW ex2 (apply (s2 `composeSubst` v1 `composeSubst` s1) env) $ OElse : os  
    v2 <- mgu (apply s3 t2) t3
    return (v2 `composeSubst` s3 `composeSubst` s2 `composeSubst` v1 `composeSubst` s1, apply v2 t2)
algoW (EIf_ cond ex1 ex2) env os = do
    (s1,t1) <- algoW cond env $ OCond : os
    (s2,t2) <- algoW ex1 (apply s1 env) $ OThen : os
    (s3,t3) <- algoW ex2 (apply s2 (apply s1 env)) $ OElse : os
    v <- mgu' [TUntyped, apply s3 (apply s2 t1), apply s3 t2,t3] 
    return (v `composeSubst` s3 `composeSubst` s2 `composeSubst` s1, TUntyped)


typeInference :: TypeEnv -> Expr -> TI Type
typeInference (TypeEnv env) e = do 
    (s, t) <- algoW e (TypeEnv env) []
    return (apply s t)

wtest :: TypeEnv -> Expr -> Expr
wtest m e =
    let (res, _) = evalTI (typeInference m e) in
    case res of
        Left err ->
            wtest m $ underline e (reverse err)
        Right t  -> e
