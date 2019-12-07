module Syntax where

import qualified Data.Map.Strict as M

data Pos = Pos  {line :: Int, col :: Int}
           deriving(Show)

data Dec
    = Tydec     { name      :: String
                , ty        :: Ty
                , pos       :: Pos
                }
    | Vardec    { name      :: String
                , tyname    :: Maybe String
                , init      :: Exp
                , pos       :: Pos
                }
    | Fundec    { name      :: String
                , args      :: [Field]
                , rettype   :: Maybe String
                , body      :: Exp
                , pos       :: Pos
                }
    deriving(Show)

data Ty
    = NameTy    String Pos
    | RecordTy  [Field] Pos
    | ArrayTy   String Pos
    deriving(Show)

data Field
    = Field     { name      :: String
                , ty        :: String
                , pos       :: Pos
                }
    deriving(Show)

data Var
    = SimpleVar String Pos
    | FieldVar  Var String Pos
    | SubscriptVar Var Exp Pos
    deriving(Show)

data Exp
    = VarExp    Var Pos
    | NilExp    Pos
    | IntExp    Integer Pos
    | StringExp String Pos
    | CallExp   { func      :: String
                , args      :: [Exp]
                , pos       :: Pos
                }
    | OpExp     { op        :: Op
                , left      :: Exp
                , right     :: Exp
                , pos       :: Pos
                }
    | RecordExp { ty        :: String
                , fields    :: M.Map String Exp
                , pos       :: Pos
                }
    | SeqExp    [Exp]
    | AssignExp { var       :: Var
                , exp       :: Exp
                , pos       :: Pos
                }
    | IfExp     { cond      :: Exp
                , true      :: Exp
                , false     :: Maybe Exp
                , pos       :: Pos
                }
    | WhileExp  { cond      :: Exp
                , body      :: Exp
                , pos       :: Pos
                }
    | ForExp    { var       :: Var
                , begin     :: Exp
                , end       :: Exp
                , body      :: Exp
                , pos       :: Pos
                }
    | BreakExp  Pos
    | LetExp    { decs      :: [Dec]
                , body      :: Exp
                , pos       :: Pos
                }
    | ArrayExp  { ty        :: String
                , size      :: Exp
                , init      :: Exp
                , pos       :: Pos
                }
    deriving(Show)

data Op
    = PlusOp
    | MinusOp
    | TimesOp
    | DivOp
    | EqOp
    | NeqOp
    | LtOp
    | LeOp
    | GtOp
    | GeOp
    deriving(Show)

data OpType
    = Arith
    | Comp
    | Eq
    deriving(Show)

optype :: Op -> OpType
optype op
    = case op of
        PlusOp  -> Arith
        MinusOp -> Arith
        TimesOp -> Arith
        DivOp   -> Arith
        GtOp    -> Comp
        GeOp    -> Comp
        LtOp    -> Comp
        LeOp    -> Comp
        EqOp    -> Eq
        NeqOp   -> Eq

