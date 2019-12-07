module Error where

import Syntax
import Type

data Error 
    = Semantic  { desc :: String
                , pos :: Pos
                }
    | Lookup    { id :: String}
    | Syntax
    deriving(Show)

unknownTypeError :: Type -> Pos -> Error
unknownTypeError t p
    = Semantic "Type is not fully qualified" p

ndefTypeError :: String -> Error
ndefTypeError id
    = Lookup ("Type " ++ id ++ " is not defined")

rdefTypeError :: String -> Error
rdefTypeError id
    = Lookup ("Type " ++ id ++ " is already defined")

ndefVarError :: String -> Error
ndefVarError id
    = Lookup ("Variable " ++ id ++ " is not defined")

ndefFuncError :: String -> Error
ndefFuncError id
    = Lookup ("Function " ++ id ++ " is not defined")

recFieldError :: Type -> String -> Pos -> Error
recFieldError t f p
    = Semantic (show t ++ " does not have field " ++ f) p

mismatchError :: Type -> Type -> Pos -> Error
mismatchError t1 t2 p
    = Semantic (show t1 ++ " does not match " ++ show t2) p
