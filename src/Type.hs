module Type where

import qualified Data.Map.Strict as M

data Type
    = IntType
    | StringType
    | NilType
    | VoidType
    | RecordType    (M.Map String Type) Int
    | ArrayType     Type Int
    | NameType      String
    | UnknownType
    deriving(Show)

instance Eq Type where
    x@IntType               == y@IntType            = True
    x@StringType            == y@StringType         = True
    x@NilType               == y@NilType            = True
    x@(RecordType _ idl)    == y@(RecordType _ idr) = idl == idr
    x@(ArrayType _ idl)     == y@(ArrayType _ idr)  = idl == idr
    x@VoidType              == y@VoidType           = True
    x@(NameType idl)        == y@(NameType idr)     = idl == idr
    _                       == _                    = False

--baseType :: Type -> Type
--baseType t
--    = case t of
--        NameType _ nbt -> baseType nbt
--        _ -> t

checkType :: Type -> Type -> Bool
checkType tl tr
    = if tl /= tr
      then case (tl, tr) of
        (RecordType _ _, NilType) -> True
        (NilType, RecordType _ _) -> True
        _ -> False
      else True
