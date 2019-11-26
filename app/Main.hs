module Main where

import Parser
import Text.Parsec (runParser)
import System.Environment
import Control.Monad
import Data.Either

--main :: IO ()
main = do
    [file] <- getArgs
    source <- readFile file
    let p = runParser expr () file source
    when (isLeft p) (error $ show p)
    putStrLn $ show p
