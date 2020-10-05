module Main where

import Common
import Parser
import AlgoM
import AlgoW
import Testing

import Test.QuickCheck.Gen
import Criterion
import Criterion.Types
import Criterion.Main

main :: IO ()
main = do
    str  <- getLine
    str' <- getLine
    case (parseExpr str,parseEnvTest str') of
        (Right ex,Right tenv) -> do
            putStr "\n<<< ================================ >>>\n"
            print ex
            print tenv
            putStr "\n<<< processing... >>>\n"
            print $ wtest tenv ex
            putStr "\n<<< processing with alternative algo >>>\n"
            print $ mtest tenv ex
        (Left err,_) -> error $ show err

{-
main :: IO ()
main = defaultMainWith
    (defaultConfig {reportFile = Just "m-versus-w.html"})
    [env (generate $ genTerm 100)
        (\e ->
            bgroup
            "m versus w/100"
            [bench "m" $ nf (mtest emptyEnv) e
            ,bench "w" $ nf (wtest emptyEnv) e])
    ,env (generate $ genTerm 200)
        (\e ->
            bgroup
            "m versus w/250"
            [bench "m" $ nf (mtest emptyEnv) e
            ,bench "w" $ nf (wtest emptyEnv) e])
    ,env (generate $ genTerm 500)
        (\e ->
            bgroup
            "m versus w/500"
            [bench "m" $ nf (mtest emptyEnv) e
            ,bench "w" $ nf (wtest emptyEnv) e])
    ,env (generate $ genTerm 1000)
    (\e ->
        bgroup
        "m versus w/1000"
        [bench "m" $ nf (mtest emptyEnv) e
        ,bench "w" $ nf (wtest emptyEnv) e])]
-}