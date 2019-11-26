module Semant where

import Syntax
import Environment

import Data.List
import qualified Data.Map as M

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

transExp :: Env -> Exp -> Type
transExp _ (NilExp _)                   = NilType
transExp _ (IntExp _ _)                 = IntType
transExp _ (StringExp _ _)              = StringType
transExp env (SeqExp exps)              =
    let
        ety = fmap (transExp env) exps
    in
        if null exps
        then VoidType
        -- Use foldl' to force the strict evaluation of all exp type
        else foldl' (\_ t -> t) VoidType ety
transExp _ (BreakExp _)                 = VoidType
transExp env (ArrayExp ty size val _)   =
    let
        sizety  = transExp env size
        valty   = transExp env val
        arrty   = baseType $ lookupType env ty
    in
        case arrty of
            (ArrayType t _) ->
                if checkType IntType sizety
                then if checkType t valty
                     then arrty
                     else error "Array type does not match init value"
                else error "Array size must be an integer"
            _ -> error $ show ty ++ " is not an array type"
transExp env (RecordExp ty values _)    =
    let
        recty = baseType $ lookupType env ty
        valty = M.map (transExp env) values
    in
        case recty of
            (RecordType types _) ->
                if (checkFields valty types)
                then recty
                else error "Record fields and init expression types does not match"
            _ -> error $ show ty ++ " is not a record type"
    where
        checkFields valty types
            = if M.difference valty types /= M.empty
              then error "Record type and expression does not have macthing fields"
              else M.foldl' (&&) True (M.intersectionWith (checkType) valty types)
transExp env (OpExp op l r _)           =
    let
        tl = transExp env l
        tr = transExp env r
    in
        case optype op of
            Arith   -> arith tl tr
            Comp    -> comp tl tr
            Eq      -> eq tl tr
    where
        arith tl tr = if (checkType tl tr) && tl == IntType
                      then IntType
                      else error "Type error for arithmetic operation expected Int"
        comp tl tr = if (checkType tl tr)
                     then if tl == IntType || tl == StringType
                          then IntType
                          else error "Type error for comparaison expected Int or String"
                     else error "Comparason between two different types"
        eq tl tr = if (checkType tl tr) && tl /= VoidType
                   then tl
                   else error "Type mismatch for equality check"
transExp env (IfExp cond true false _)  =
    let
        condty = transExp env cond
        truety = transExp env true
    in
        if checkType IntType condty
        then case false of
            Just false' -> if checkType truety (transExp env false')
                           then truety
                           else error "Then and else do not have consistant type in if expression"
            Nothing     -> if checkType VoidType truety
                           then VoidType
                           else error "If statement without else must not produce any value"
        else error "If condition must be an integer"
transExp env (WhileExp cond body _) =
    let
        condty = transExp env cond
        bodyty = transExp env body
    in
        if checkType IntType condty
        then if checkType VoidType bodyty
             then VoidType
             else error "While body must not produce any value"
        else error "While condition must be an integer"
transExp env (ForExp var beg end body _) =
    let
        varName (SimpleVar id _) = id
        varName _ = error "For variable should be an integer"
        forenv = insertVar env (varName var) IntType
        begty = transExp forenv beg
        endty = transExp forenv end
        bodyty = transExp forenv body
    in
        if checkType IntType begty &&
           checkType IntType endty
        then if checkType VoidType bodyty
             then VoidType
             else error "For loop must not produce any value"
        else error "For variable and bounds must be integers"
transExp env (AssignExp var val _)      =
    let
        varty = transVar env var
        valty = transExp env val
    in
        if checkType varty valty
        then VoidType
        else error $ show varty ++ " does not match " ++ show valty ++ " type"
transExp env (CallExp func args _)      =
    let
        (paramty, retty) = lookupFunc env func
        argsty = map (transExp env) args
    in
        if checkArgs argsty paramty
        then baseType retty
        else error $ show func ++ " arguments type mismatch"
    where
        checkArgs :: [Type] -> [Type] -> Bool
        checkArgs argsty paramty
            = if (length argsty) == (length paramty)
              then foldl' (&&) True (zipWith checkType argsty paramty)
              else error $ "Missing arguments in " ++ func ++ " call"
transExp env (VarExp var _)             =
    transVar env var
transExp env (LetExp decs body _)       =
    let
        newenv = foldl' transDec env decs
    in
        --error $ show newenv
        newenv `seq` transExp newenv body

transVar :: Env -> Var -> Type
transVar env (SimpleVar v _)            = baseType $ lookupVar env v
transVar env (FieldVar v field _)       =
    let
        vty = transVar env v
    in
        case vty of
            (RecordType fields _) ->
                case M.lookup field fields of
                    Just t -> baseType t
                    Nothing -> error $ field ++ " is not a member of " ++ show vty
            _ -> error $ show v ++ " is not a record"
transVar env (SubscriptVar v index _)   =
    let
        indexty = transExp env index
        vty = transVar env v
    in
        case vty of
            (ArrayType t _) ->
                if checkType IntType indexty
                then baseType t
                else error "Array index should be an integer"
            _ -> error $ show v ++ " is not an array"

transDec :: Env -> Dec -> Env
transDec env (Tydec name tydec _)       =
    let
        ty = transTy env tydec
    in
        insertType env name ty
transDec env (Vardec name tydec val _)  =
    let
        valty = transExp env val
    in
        case tydec of
            Just t ->
                if checkType valty (lookupType env t)
                then insertVar env name valty
                else error $ show name ++ "'s type differs from its init value's type"
            Nothing -> valty `seq` insertVar env name valty
transDec env (Fundec name args ret body _) =
    let
        funcenv = foldl' (\e (Field id t _) -> insertVar e id (lookupType env t)) env args
        bodyty = transExp funcenv body
        argsty = map (\(Field _ t _) -> lookupType env t) args
    in
        case ret of
            Just t ->
                if checkType bodyty (lookupType env t)
                then insertFunc env name argsty bodyty
                else error $ show name ++ " should return " ++ show t ++ "but return " ++ show bodyty
            Nothing -> insertFunc env name argsty bodyty

transTy :: Env -> Ty -> Type
transTy env (NameTy ty _)               =
    NameType (Just (lookupType env ty))
transTy env (RecordTy fields _)         =
    let
        fieldsty = map (\(Field n t _) -> (n, lookupType env t)) fields
    in
        RecordType (M.fromList fieldsty) (newTypeId env)
transTy env (ArrayTy ty _)              =
    ArrayType (lookupType env ty) (newTypeId env)

