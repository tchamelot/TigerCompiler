module Main where

import Parser
import Semant
import Environment
import Text.Parsec (runParser)
import System.Environment
import Control.Monad
import Data.Either

--main :: IO ()
main = do
    [file] <- getArgs
    source <- readFile file
    let p = runParser expr () file source
    case p of
        Left e -> error $ show e
        Right e -> putStrLn $ show $ transExp tigerEnv e
