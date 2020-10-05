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