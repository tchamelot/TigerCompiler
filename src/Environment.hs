module Environment where

import qualified Data.Map.Strict as M

-- Type identifiers
data Type
    = IntType
    | StringType
    | NilType
    | VoidType
    | RecordType    (M.Map String Type) Int
    | ArrayType     Type Int
    | NameType      (Maybe Type)
    deriving(Show)

instance Eq Type where
    x@IntType               == y@IntType            = True
    x@StringType            == y@StringType         = True
    x@NilType               == y@NilType            = True
    x@(RecordType _ idl)    == y@(RecordType _ idr) = idl == idr
    x@(ArrayType _ idl)     == y@(ArrayType _ idr)  = idl == idr
    x@VoidType              == y@VoidType           = True
    x@(NameType tl)         == y@(NameType tr)      = tl == tr
    _                       == _                    = False

baseType :: Type -> Type
baseType t
    = case t of
        NameType mbt -> case mbt of
            Just bt -> baseType bt
            Nothing -> error "Undefined Type"
        _ -> t

checkType :: Type -> Type -> Bool
checkType tl tr
    = if btl /= btr
      then case (btl, btr) of
        (RecordType _ _, NilType) -> True
        (NilType, RecordType _ _) -> True
        _ -> False
      else True
      where
        btl = baseType tl
        btr = baseType tr

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
        Nothing -> error $ "Unknown type " ++ key

insertType :: Env -> String -> Type -> Env
insertType (Env types tids ids) key ty
    = Env newtypes newtids ids
    where
        newtids = case ty of
            ArrayType _ tid -> tid
            RecordType _ tid -> tid
            _ -> tids
        newtypes = M.insertWithKey errIfExist key ty types
        errIfExist k _ _ = error $ "Redifinition of " ++ k 

newTypeId :: Env -> Int
newTypeId (Env _ tids _) = tids + 1

lookupVar :: Env -> String -> Type
lookupVar (Env _ _ ids) key
    = case M.lookup key ids of
        Just v@(VarId t) -> t
        Just v@(FuncId _ _) -> error $ key ++ " is a function not a variable"
        Nothing -> error $ "Unknown variable " ++ key

insertVar :: Env -> String -> Type -> Env
insertVar (Env types tids ids) key ty
    = Env types tids newids
    where
        newids = M.insert key (VarId ty) ids

lookupFunc :: Env -> String -> ([Type], Type)
lookupFunc (Env _ _ ids) key
    = case M.lookup key ids of
        Just v@(FuncId args ret) -> (args, ret)
        Just v@(VarId _) -> error $ key ++ " is a variable not a function"
        Nothing -> error $ "Unknow function " ++ key

insertFunc :: Env -> String -> [Type] -> Type -> Env
insertFunc (Env types tids ids) key args ret
    = Env types tids newids
    where
        newids = M.insertWithKey errIfExist key (FuncId args ret) ids
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
