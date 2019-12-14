import Semant
import Environment
import Parser
import Type

import Text.Parsec (runParser)
import System.Environment
import Control.Monad
import Data.List
import Data.Either
import Data.Traversable
import qualified Data.Map.Strict as M

testPath n = "/home/tchamelot/workspace/compiler/TigerCompiler/testcases/test" ++ show n ++ ".tig"

test = [1..48]

-- [(Type, Error)]
expected = [(ArrayType IntType 1525, 0),
            (ArrayType IntType 2025, 0),
            (RecordType (M.fromList [("name", StringType), ("age", IntType)]) 1525, 0),
            (IntType, 0),
            (RecordType (M.fromList [("hd", IntType), ("tl", NameType "intlist")]) 2016, 0),
            (VoidType, 0),
            (IntType, 0),
            (IntType, 0),
            (UnknownType, 1),
            (VoidType, 1),
            (VoidType, 2),
            (VoidType, 0),
            (IntType, 1),
            (IntType, 1),
            (VoidType, 1),
            (StringType, 4),
            (IntType, 0),
            (IntType, 0),
            (IntType, 1), -- TODO Fix this test which should not be ok
            (VoidType, 2),
            (VoidType, 2),
            (VoidType, 2),
            (VoidType, 2),
            (UnknownType, 1),
            (UnknownType, 1),
            (IntType, 1),
            (IntType, 0),
            (UnknownType, 2),
            (UnknownType, 2),
            (IntType, 0),
            (UnknownType, 2),
            (IntType, 1),
            (IntType, 1),
            (IntType, 1),
            (IntType, 1),
            (IntType, 1),
            (IntType, 0),
            (IntType, 1),
            (IntType, 1),
            (VoidType, 1),
            (IntType, 0),
            (VoidType, 0),
            (IntType, 1),
            (VoidType, 0),
            (UnknownType, 1),
            (IntType, 0),
            (IntType, 1),
            (IntType, 1)] -- not handled yet
            

checkResult :: (Type, Int) -> Expty -> String
checkResult exp@(et, nerr) res@(Expty rt errs) =
    if et == rt && nerr == length errs
    then " Ok"
    else " Expected " ++ show exp ++ ", got " ++ show res

runTest source =
    let 
        p = runParser expr () "" source
    in
        case p of
            Left e -> error $ show e
            Right e -> transExp tigerEnv e

main :: IO ()
main = do
    sources <- traverse (readFile . testPath) test
    let batch = zip expected (fmap runTest sources)
    putStrLn "\nTest results:\n"
    let results = zip [1..] (fmap (\(x, y) -> checkResult x y) batch)
    putStrLn $ intercalate "\n" (fmap (\(i, r) -> show i ++ r) results)

