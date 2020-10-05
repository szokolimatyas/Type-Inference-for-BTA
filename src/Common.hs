{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


module Common where

import qualified Data.Map as Map
import Data.Map(Map(..))
import qualified Data.Set as Set
import Data.Set(Set(..))
import Data.Maybe
import Control.Monad.Except
import Control.Monad.State
import GHC.Generics (Generic)
import Control.DeepSeq

data Occurrence = OFun 
                | OArg 
                | OBody 
                | OCond 
                | OElse 
                | OThen
                | OFixBody
    deriving (Eq,Show)

type TyVar = Int

data Type = TBase [Occurrence]
            | TArrow Type Type [Occurrence]
            | TVar TyVar
            | TUntyped
    deriving (Eq,Show)

type Subst = Map TyVar Type

type TI a =  ExceptT [Occurrence] (State TyVar) a

newtype TypeEnv = TypeEnv (Map String Type)
    deriving(Show)

class Types a where
    ftv :: a -> Set TyVar
    apply :: Subst -> a -> a 

instance Types Type where
    ftv (TVar n) = Set.singleton n
    ftv (TBase _) = Set.empty 
    ftv (TArrow t1 t2 _) = Set.union (ftv t1) (ftv t2)
    ftv TUntyped = Set.empty

    apply s (TVar n) = fromMaybe (TVar n) (Map.lookup n s)    
    apply s (TArrow t1 t2 o) = TArrow (apply s t1) (apply s t2) o
    apply _ t = t

instance Types a => Types [a] where
    apply s = map $ apply s
    ftv = foldr (Set.union . ftv) Set.empty

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv $ Map.delete var env

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv $ Map.elems env
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

varBind :: Int -> Type -> TI Subst
varBind n t@(TArrow _ _ os) = 
    if Set.member n (ftv t) 
        then throwError os -- occurs check
        else return $ Map.singleton n t
varBind n t = return $ Map.singleton n t


mgu :: Type -> Type -> TI Subst
mgu (TBase _) (TBase _) = return nullSubst
mgu TUntyped TUntyped = return nullSubst
mgu (TVar n) t = varBind n t
mgu t (TVar n) = varBind n t
mgu (TArrow t1 t2 _) (TArrow t1' t2' _) = do
    s1 <- mgu t1 t1'
    s2 <- mgu (apply s1 t2) (apply s1 t2')
    return $  s2 `composeSubst` s1
mgu (TBase os) _ = throwError os
mgu _ (TBase os) = throwError os
mgu (TArrow _ _ os) _ = throwError os
mgu _ (TArrow _ _ os) = throwError os

mgu' :: [Type] -> TI Subst
mgu' (x:y:ys)  = do
    s1 <- mgu x y
    s2 <- mgu' $ apply s1 y:ys
    return $ s2 `composeSubst` s1
mgu' _ = return nullSubst

data Expr = EApp Expr Expr
            | EAbs String Expr
            | EFix Expr
            | EIf Expr Expr Expr
            | EConst

            | EApp_ Expr Expr
            | EAbs_ String Expr
            | EFix_ Expr
            | EIf_ Expr Expr Expr
            | EConst_
            | EVar String
            | EVar_ String
    deriving (Eq,Show,NFData,Generic)

freshTyVar :: TI Type
freshTyVar = do
    i <- get
    modify (+1)
    return $ TVar i

evalTI :: TI a -> (Either [Occurrence] a,Int)
evalTI t = 
    let e  = runExceptT t  
        e' = runState e 0
    in e'

underline :: Expr -> [Occurrence] -> Expr
underline (EApp ex1 ex2) (OFun : os)       = EApp (underline ex1 os) ex2
underline (EApp_ ex1 ex2) (OFun : os)      = EApp_ (underline ex1 os) ex2
underline (EApp ex1 ex2) (OArg : os)       = EApp ex1 (underline ex2 os)
underline (EApp_ ex1 ex2) (OArg : os)      = EApp_ ex1 (underline ex2 os)
underline (EAbs n ex) (OBody : os)         = EAbs n (underline ex os)
underline (EAbs_ n ex) (OBody : os)        = EAbs_ n (underline ex os)
underline (EIf ex1 ex2 ex3) (OCond : os)   = EIf (underline ex1 os) ex2 ex3
underline (EIf_ ex1 ex2 ex3) (OCond : os)  = EIf_ (underline ex1 os) ex2 ex3
underline (EIf ex1 ex2 ex3) (OThen : os)   = EIf ex1 (underline ex2 os) ex3
underline (EIf_ ex1 ex2 ex3) (OThen : os)  = EIf_ ex1 (underline ex2 os) ex3
underline (EIf ex1 ex2 ex3) (OElse : os)   = EIf ex1 ex2 (underline ex3 os)
underline (EIf_ ex1 ex2 ex3) (OElse : os)  = EIf_ ex1 ex2 (underline ex3 os)

underline (EApp ex1 ex2) [] = EApp_ ex1 ex2
underline (EAbs n ex) [] = EAbs_ n ex
underline (EIf ex1 ex2 ex3) [] = EIf_ ex1 ex2 ex3
underline EConst [] = EConst_
underline (EVar n) [] = EVar_ n
underline ex os = error $ "internal error: " ++ show ex ++ " " ++ show os
