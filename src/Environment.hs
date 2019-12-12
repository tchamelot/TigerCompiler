module Environment where

import Error
import Type

import qualified Data.Map.Strict as M

-- Variable and Function identifiers
data Identifier
    = VarId Type
    | FuncId [Type] Type
    deriving(Show)

-- Semantic checking environment
data Env
    = Env   { types :: M.Map String Type
            , tids  :: Int
            , ids   :: M.Map String Identifier
            }
     deriving(Show)

lookupType :: Env -> String -> Type
lookupType (Env types _ _) key
    = case M.lookup key types of
        Just t  -> t
        Nothing -> UnknownType

insertType :: Env -> String -> Type -> Either Error Env
insertType (Env types tids ids) key ty
    = case ty of
        UnknownType -> Right (Env (M.insert key ty types) tids ids)
        _ -> 
            case M.insertLookupWithKey (\_ t _ -> t) key ty types of
                (Nothing, types') -> Right (Env types' newtids ids)
                (Just t, types') -> 
                    case t of
                        UnknownType -> Right (Env types' newtids ids)
                        (NameType _) -> Right (Env types' newtids ids)
                        _ -> Left (rdefTypeError key)
    where
        newtids = case ty of
            ArrayType _ tid -> tid
            RecordType _ tid -> tid
            _ -> tids

newTypeId :: Env -> Int
newTypeId (Env _ tids _) = tids + 1

baseType :: Env -> Type -> Type
baseType env t =
    case t of
        NameType id -> checkLoop t $ lookupType env id
        _ -> t
    where
        checkLoop t1 t2 =
            if t1 == t2
            then UnknownType
            else
                case t2 of
                    NameType id -> checkLoop t1 $ lookupType env id
                    _ -> t2

lookupVar :: Env -> String -> Either Error Type
lookupVar (Env _ _ ids) key
    = case M.lookup key ids of
        Just v@(VarId t) -> Right t
        Just v@(FuncId _ _) -> Left (Lookup ("Id " ++ key ++ " is a function, not a variable"))
        Nothing -> Left (ndefVarError key)

insertVar :: Env -> String -> Type -> Env
insertVar (Env types tids ids) key ty
    = Env types tids newids
    where
        newids = M.insert key (VarId ty) ids

lookupFunc :: Env -> String -> Either Error ([Type], Type)
lookupFunc (Env _ _ ids) key
    = case M.lookup key ids of
        Just v@(FuncId args ret) -> Right (args, ret)
        Just v@(VarId _) -> Left (Lookup ("Id " ++ key ++ " is a variable, not a function"))
        Nothing -> Left (ndefFuncError key)

insertFunc :: Env -> String -> [Type] -> Type -> Env
insertFunc (Env types tids ids) key args ret
    = case args of
        [UnknownType] -> Env types tids newids
        _ -> Env types tids newidsErr
    where
        newids = M.insert key (FuncId args ret) ids
        newidsErr = M.insertWithKey errIfExist key (FuncId args ret) ids
        errIfExist k _ _ = error $ "Redifinition of " ++ k 

tigerTypes :: M.Map String Type
tigerTypes = M.fromList([("int", IntType), ("string", StringType)])

tigerFuncs :: M.Map String Identifier
tigerFuncs = M.fromList([("print",      FuncId [StringType] VoidType),
                         ("flush",      FuncId [] VoidType),
                         ("getchar",    FuncId [] StringType),
                         ("ord",        FuncId [StringType] IntType),
                         ("chr",        FuncId [IntType] StringType),
                         ("size",       FuncId [StringType] IntType),
                         ("substring",  FuncId [StringType, IntType, IntType] StringType),
                         ("concat",     FuncId [StringType, StringType] StringType),
                         ("not",        FuncId [IntType] IntType),
                         ("exit",       FuncId [IntType] VoidType)
                        ])

tigerEnv :: Env
tigerEnv = Env tigerTypes 0 tigerFuncs

