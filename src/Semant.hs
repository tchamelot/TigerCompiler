module Semant where

import Syntax
import Type
import Environment
import Error

import Data.List
import Data.Either
import qualified Data.Map as M

data Expty = Expty Type [Error]
             deriving(Show)

merge :: Expty -> Expty -> Expty
merge (Expty _ preve) (Expty ty newe)
    = Expty ty (preve ++ newe)

transExp :: Env -> Exp -> Expty
transExp _      (NilExp _)              = Expty NilType []
transExp _      (IntExp _ _)            = Expty IntType []
transExp _      (StringExp _ _)         = Expty StringType []
transExp env    (SeqExp exps)           =
    let
        expsty = fmap (transExp env) exps
    in
        -- Use foldl' to force the strict evaluation of all exp type
        foldl' merge (Expty VoidType []) expsty
transExp _      (BreakExp _)            = Expty VoidType []
transExp env    (ArrayExp ty s v p)     =
    let
        Expty sty serr = transExp env s
        Expty vty verr = transExp env v
        errs = serr ++ verr
        arrty = lookupType env ty
    in
        case baseType env arrty of
            bt@(ArrayType t _) ->
                if checkType IntType sty
                then if checkType (baseType env t) vty
                     then Expty bt errs
                     else Expty bt (errs ++ [mismatchError vty t p])
                else Expty bt (errs ++ [mismatchError sty IntType p])
            _ -> Expty UnknownType (errs ++ [mismatchError arrty (ArrayType vty 0) p])
transExp env    (RecordExp ty v p)      =
    let
        vty = M.map (transExp env) v
        recty = lookupType env ty
    in
        case baseType env recty of
            bt@(RecordType tys _) ->
                let
                    dty = fmap (\t -> Expty (baseType env t) []) tys
                in
                    Expty bt (M.foldlWithKey' handleErrors [] (checkFields vty dty))
            _ -> Expty UnknownType [mismatchError recty (RecordType (M.fromList []) 0) p]
    where
        checkFields f1 f2 = M.differenceWith checkField f1 f2
        checkField (Expty t1 e1) (Expty t2 _) =
            if checkType t1 t2
            then if null e1
                 then Nothing
                 else Just (Expty t1 e1)
            else Just (Expty t2 (e1 ++ [mismatchError t1 t2 p]))
        handleErrors errs k (Expty _ e) =
            if null e
            then errs ++ [Semantic ("Field " ++ k ++ " is not initialized") p]
            else errs ++ e
transExp env    (OpExp op l r p)        =
    let
        tl = transExp env l
        tr = transExp env r
    in
        case optype op of
            Arith   -> arith tl tr
            Comp    -> comp tl tr
            Eq      -> eq tl tr
    where
        arith (Expty tl el) (Expty tr er) = 
            if (checkType tl tr) && tl == IntType
            then if checkType IntType tl
                 then Expty IntType (el ++ er)
                 else Expty IntType (el ++ er ++ [mismatchError tl IntType p])
            else Expty IntType (el ++ er ++  [mismatchError tl tr p])
        comp (Expty tl el) (Expty tr er) =
            if (checkType tl tr)
            then if tl == IntType || tl == StringType
                 then Expty IntType (el ++ er)
                 else Expty IntType (el ++ er ++ [Semantic "Only string and in are comparable" p])
            else Expty IntType (el ++ er ++ [mismatchError tl tr p])
        eq (Expty tl el) (Expty tr er) =
            if (checkType tl tr) && tl /= VoidType
            then Expty IntType (el ++ er)
            else Expty IntType (el ++ er ++ [mismatchError tl tr p])
transExp env    (IfExp c t e p)         =
    let
        Expty cty cerr = transExp env c
        Expty tty terr = transExp env t
    in
        if checkType IntType cty
        then case e of
            Just e' -> 
                let
                    Expty ety eerr = transExp env e'
                in
                    if checkType tty ety
                    then Expty tty (cerr ++ terr ++ eerr)
                    else Expty UnknownType (cerr ++ terr ++ eerr ++ [mismatchError tty ety p])
            Nothing -> 
                if checkType VoidType tty
                then Expty VoidType (cerr ++ terr)
                else Expty VoidType (cerr ++ terr ++ [mismatchError cty VoidType p])
        else Expty UnknownType (cerr ++ terr ++ [mismatchError cty IntType p])
transExp env    (WhileExp c b p)        =
    let
        Expty cty cerr = transExp env c
        Expty bty berr = transExp env b
    in
        if checkType IntType cty
        then if checkType VoidType bty
             then Expty VoidType (cerr ++ berr)
             else Expty VoidType (cerr ++ berr ++ [mismatchError bty VoidType p])
        else Expty VoidType (cerr ++ berr ++ [mismatchError cty IntType p])
transExp env    (ForExp v i e b p)      =
    let
        varName (SimpleVar id _) = id
        varName _ = error "For variable should be an integer"

        env' = insertVar env (varName v) IntType
        Expty ity ierr = transExp env' i
        Expty ety eerr = transExp env' e
        Expty bty berr = transExp env' b
        errs = ierr ++ eerr ++ berr
    in
        if checkType IntType ity &&
           checkType IntType ety
        then if checkType VoidType bty
             then Expty VoidType errs
             else Expty VoidType (errs ++ [mismatchError bty VoidType p])
        else Expty VoidType (errs ++ [mismatchError ity IntType p, mismatchError ety IntType p])
transExp env    (AssignExp var val p)   =
    let
        Expty varty varerr = transVar env var
        Expty valty valerr = transExp env val
        errs = varerr ++ valerr
    in
        if checkType varty valty
        then Expty VoidType errs
        else Expty VoidType (errs ++ [mismatchError valty varty p])
transExp env    (CallExp f args p)      =
    let
        argsty = map (transExp env) args
    in
        case lookupFunc env f of
            Right (paramty, retty) ->
                let
                    Expty _ argserr = checkArgs paramty argsty 
                in
                    Expty (baseType env retty) argserr
            Left (Lookup err) -> Expty UnknownType [Semantic err p]
    where
        checkArg t1 (Expty t2 err) = 
            if checkType t1 t2
            then Expty t1 []
            else Expty t1 [mismatchError t1 t2 p]
        checkArgs t1 t2 =
            if (length t1) == (length t2)
            then foldl' merge (Expty UnknownType []) (zipWith checkArg t1 t2)
            else Expty UnknownType [Semantic ("Function " ++ f ++ " call does not match definition") p]
transExp env    (VarExp var _)          =
    transVar env var
transExp env    (LetExp d b _)          =
    let
        (preenv, err1) = foldl' (mergeEnv transDecHeader) (env, []) d
        (newenv, err2) = foldl' (mergeEnv transDec) (preenv, err1) d
    in
        merge (Expty UnknownType err2) (transExp newenv b)
    where
        mergeEnv f (env, err) desc =
            let
                (newenv, newerr) = f env desc
            in
                (newenv, err ++ newerr)
                
        

transVar :: Env -> Var -> Expty
transVar env    (SimpleVar v p)         = 
    case lookupVar env v of
        Right t -> Expty (baseType env t) []
        Left (Lookup err) -> Expty UnknownType [Semantic err p]
transVar env    (FieldVar v field p)    =
    let
        Expty vty verr = transVar env v
    in
        case vty of
            (RecordType fields _) ->
                case M.lookup field fields of
                    Just t -> Expty (baseType env t) verr
                    Nothing -> Expty UnknownType (verr ++ [recFieldError vty field p])
            _ -> Expty UnknownType (verr ++ [mismatchError vty (RecordType M.empty 0) p])
transVar env    (SubscriptVar v i p)    =
    let
        Expty ity ierr = transExp env i
        Expty vty verr = transVar env v
        errs = ierr ++ verr
    in
        case vty of
            (ArrayType t _) ->
                if checkType IntType ity
                then Expty (baseType env t) errs
                else Expty (baseType env t) (errs ++ [mismatchError ity IntType p])
            _ -> Expty UnknownType (errs ++ [mismatchError vty (ArrayType UnknownType 0) p])

transDecHeader :: Env -> Dec -> (Env, [Error])
transDecHeader env  (Tydec t _ p)           =
    case insertType env t (NameType t) of
        Right newenv -> (newenv, [])
        Left (Lookup err) -> (env, [Semantic err p])
transDecHeader env  (Fundec f a r _ p)      =
    let
        aty = map (\(Field _ t _) -> lookupType env t) a
        rty = maybe VoidType (lookupType env) r
    in
        if rty /= UnknownType
        then 
            case elemIndex UnknownType aty of
                Nothing -> (insertFunc env f aty rty, [])
                Just i -> (insertFunc env f aty UnknownType, [Semantic ("Type of argmument " ++ show i ++ " is not defined") p])
        else (insertFunc env f [UnknownType] UnknownType, [Semantic "Return type not defined" p])
transDecHeader env _                        =
    (env, [])

transDec :: Env -> Dec -> (Env, [Error])
transDec env (Tydec name tydec p)       =
    let
        Expty ty err = transTy env tydec
    in
        case insertType env name ty of
            Right newenv -> (newenv, err)
            Left (Lookup err) -> (env, [Semantic err p])
transDec env (Vardec name tydec val p)  =
    let
        Expty vty verr = transExp env val
        t = maybe vty (baseType env . lookupType env) tydec
    in
        if checkType t vty
        then (insertVar env name t, verr)
        else (env, verr ++ [mismatchError t vty p])
transDec env (Fundec name args ret body p) =
    let
        funcenv = foldl' (\e (Field id t _) -> insertVar e id (lookupType env t)) env args
        Expty bodyty bodyerr = transExp funcenv body
        argsty = map (\(Field _ t _) -> lookupType env t) args
        rty = maybe VoidType (lookupType env) ret
    in
        if checkType bodyty rty
        then (env, bodyerr)
        else (env, bodyerr ++ [mismatchError bodyty rty p]) 

transTy :: Env -> Ty -> Expty
transTy env (NameTy ty p)               =
    case baseType env $ lookupType env ty of
        UnknownType -> Expty (UnknownType) [Semantic "Unknown type error" p]
        t -> Expty t []
transTy env (RecordTy fields p)         =
    let
        fieldsty = map (\(Field n t _) -> (n, lookupType env t)) fields
        tid = hashPos p
    in
        case elemIndex UnknownType (snd $ unzip $ fieldsty) of
            Nothing -> Expty (RecordType (M.fromList fieldsty) tid) []
            Just i ->  Expty (RecordType (M.fromList fieldsty) tid) [Semantic ("Type of field " ++ show i ++ " is not defined") p]
transTy env (ArrayTy ty p)              =
    let
        tid = hashPos p
    in
        case lookupType env ty of
            UnknownType -> Expty (ArrayType (lookupType env ty) tid) [Semantic "Unknown type error" p]
            _ -> Expty (ArrayType (lookupType env ty) tid) []

hashPos :: Pos -> Int
hashPos (Pos l c) = l * 500 + c
