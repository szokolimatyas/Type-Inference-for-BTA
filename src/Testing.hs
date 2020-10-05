module Testing where

import AlgoM
import AlgoW
import Common
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Data.List
import Data.Map.Internal(toList)
import Control.Monad.Memo
import Data.Functor.Identity


type MemoCount = MemoT (Int,Int) Int Gen

-- lots of parts can be reworked to only use Applicative+Functor instead of Monad
count' :: (Int,Int) -> MemoCount Int
count' (n, f) 
    | n == 0 = return 0
    | n == 1 = return f
    | otherwise = (+) <$> countLams' (n, f) <*> countApps' (n, f)

countLams' :: (Int,Int) -> MemoCount Int    
countLams' (n, f) = memo count' (n - 1, f + 1)

countApps' :: (Int,Int) -> MemoCount Int
countApps' (n, f) 
    | even n = 
        foldM sumM 0 [1..((n-2) `div` 2)]
    | otherwise = do
        c1 <- foldM sumM 0 [1..((n-3) `div` 2)] 
        c2 <- memo count' ((n - 1) `div` 2, f)   
        return $ c1 + (c2^2)
    where
        sumM acc i = do
            c1 <- memo count' (i, f)
            c2 <- memo count' (n - 1 - i, f)
            return $ acc + 2 * c1 * c2

-- from Control.Monad.Extra
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)

random :: Int -> Gen Int
random n = choose (0, n-1)

genTerm :: Int -> Gen Expr
genTerm n = startEvalMemoT $ gen n 0

gen :: Int -> Int -> MemoCount Expr
gen n f 
    | n == 0 = error "Internal error in gen, tried to gen with n==0"
    | (n == 1) && (f > 0) = do
        r <- lift $ random n
        return $ EVar $ show r
    | n == 2 = genLamTerm n f
    | otherwise = do
        rC <- memo count' (n, f)
        r <- lift $ random rC
        appC <- countApps' (n, f)
        if r < appC 
            then genAppTerm n f
            else genLamTerm n f


genLamTerm :: Int -> Int -> MemoCount Expr
genLamTerm n f = gen (n-1) (f+1) >>= (return . EAbs (show f))

genAppTerm :: Int -> Int -> MemoCount Expr
genAppTerm n f = do
    n1 <- nOne n f
    l <- gen n1 f
    r <- gen (n - 1 - n1) f
    return $ EApp l r 

randNum :: Int -> Int -> MemoCount Int
randNum n f = do
    r <- countApps' (n, f) 
    lift $ random r

nOne :: Int -> Int -> MemoCount Int
nOne n f = do
    r <- randNum n f
    rhs1 <- memo count' (1, f)
    rhs2 <- memo count' (n-1, f)
    if 0 <= r && r < rhs1 *  rhs2
        then return 1
        else do
            ret <- findM (search r n f) [1..n-2]
            case ret of
                Just i -> return i
                _      -> error "internal error in nOne"
    where
        search :: Int -> Int -> Int -> Int -> MemoCount Bool
        search rand n f i = do
            lhs  <- foldM sumM 0 [1..i-1] 
            rhs1 <- memo count' (i, f)
            rhs2 <- memo  count' (n-1-i, f)
            return $ lhs <= rand && rand < (lhs + rhs1 * rhs2)

        sumM acc j = do
            c1 <- memo count' (j, f)
            c2 <- memo count' (n-1-j, f)
            return $ acc + c1 * c2
        
instance Arbitrary Expr where 
    arbitrary = genTerm 15

quietCheckResult :: Testable prop => prop -> IO Result
quietCheckResult = quickCheckWithResult args
  where args = stdArgs { chatty = False }

